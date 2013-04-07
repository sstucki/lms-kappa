package kappa

import scala.language.implicitConversions

import scala.collection.mutable


trait Actions {
  this: LanguageContext
      with SiteGraphs
      with Patterns
      with Mixtures
      with RollbackMachines
      with Embeddings
      with PartialEmbeddings
      with Rules =>

  // --- Actions ---

  /**
   * A class representing actions.
   *
   * TODO: Should we replace the pre/post condition functions by
   * abstract methods of this class?  If yes, how should "undefined"
   * pre/post conditions be handled?  Simply defining them to return
   * the constant `true` does not easily allow us to check whether we
   * need to checkpoint the mixture before applying the action.
   * Always checkpointing the mixture would lead to performance
   * degradations in cases where the preconditions always hold.
   *
   * @param lhs the left-hand side of the action.
   * @param rhs the right-hand side of the action.
   * @param pe the partial embedding of this action.
   * @param rhsAgentOffsets a map containing the offsets in the
   *        agents array of the agents in the RHS.
   * @param preCondition an optional predicate to execute ''before''
   *        applying the action.  If `preCondition` is `Some(f)` and
   *        if `f` applied to the agents array returns `false`, the
   *        action will not be applied and the mixture remains
   *        untouched.
   * @param postCondition a predicate to execute ''after'' applying
   *        the action.  If `postCondition` is `Some(f)` and `f`
   *        applied to the agents array returns `false` the action
   *        application will be canceled and the state of the mixture
   *        prior to the action application will be restored.
   */
  final class Action(
    val lhs: Pattern,
    val rhs: Pattern,
    val pe: PartialEmbedding,
    val rhsAgentOffsets: Map[Pattern.Agent, AgentIndex],
    val preCondition:  Option[(Action, Action.Agents) => Boolean] = None,
    val postCondition: Option[(Action, Action.Agents) => Boolean] = None)
      extends Function2[Embedding[Mixture.Agent], Mixture, (Boolean, Boolean)]
  {
    import Action._

    type ActivationEntry = Iterable[Iterable[(Pattern.Agent, AgentIndex)]]

    /** The atomic actions making up this action. */
    val atoms: Seq[Action.Atom] = mkAtoms(lhs, rhs, pe, rhsAgentOffsets)

    /** Activation map. */
    val activationMap =
      mutable.HashMap[ComponentIndex, ActivationEntry]()


    /**
     * Extend the activation map with an entry for a given component.
     *
     * Given a connected component `component`, check whether this
     * rule activates that component and if yes, add the appropriate
     * activation entry to the activation map.
     *
     * @param component the [[Patterns#Pattern.Component]] to check.
     */
    def addActivation(component: Pattern.Component) {
      val idx = component.modelIndex
      if (!(activationMap isDefinedAt idx)) {
        val activations = findActivation(component)
        if (!activations.isEmpty) {
          activationMap += ((idx, activations))

          // RHZ: This is redundant no? Or the += above has side effects?
          if (!activations.isEmpty && !activations.head.isEmpty) {
            println ("# Added activation entry for action " + lhs + " -> " +
              rhs + " on component # " + idx + " (CC " +
              component.index + " of pattern " + component.pattern + ")")
            for ((ae, i) <- activations.zipWithIndex; (u, ai) <- ae) {
              println ("#    RHS component " + i + ": " + ai +
                " --(+)--> " + u)
            }
          }
        }
      }
    }


    // -- Function2[Embedding, Mixture, Boolean] API --

    /**
     * Apply this action to a mixture along a given embedding.
     *
     * @param embedding the embedding along which the action should
     *        be applied.
     * @param mix the mixture to which the action should be applied.
     * @return `true` if the action application resulted in a
     *         productive event, `false` otherwise.
     */
    def apply(
      embedding: Embedding[Mixture.Agent],
      mixture: Mixture = mix): (Boolean, Boolean) = {

      // Check consistency and populate the agents array.
      val additions = rhs.length - pe.length
      val totalAgents = (embedding map (_.length)).sum + additions
      val agents = new Array[Mixture.Agent](totalAgents)
      val (clash, garbage) = checkConsistency(embedding, agents)
      if (garbage > 0) {
        throw new IllegalStateException(
          "# found and collected " + garbage +
          " garbage component embeddings.")
      } else if (clash) {
        println("# clash!")
        (false, true)
      } else if (!(preCondition map { f => f(this, agents) } getOrElse true)) {
        println("# pre-condition = false.")
        (false, false)
      } else {

        // If the post-condition is defined, checkpoint the mixture so
        // we can restore its current state if the post-condition
        // evaluates to `false`.
        if (!postCondition.isEmpty) mix.checkpoint

        // Clear the marked agents list of mix
        mix.clearMarkedAgents(Updated)
        mix.clearMarkedAgents(SideEffect)

        // Apply all atomic actions
        for (a <- atoms) a(agents, mixture)

        val mas = mix.markedAgents(Updated)

        performUpdates(agents, mas)

        // Check post-conditions
        if (postCondition map { f => f(this, agents) } getOrElse true) {
          // Discard pre-application checkpoint and perform
          // negative/positive updates.
          if (!postCondition.isEmpty) mix.discardCheckpoint
          (true, false)
        } else {
          // Roll back to the state of the mixture prior to the action
          // application.
          println("# post-condition = false.")
          mix.rollback
          (false, false)
        }
      }
    }


    // -- Any API --
    override def toString: String = atoms.toString


    // -- Protected/private methods --

    /**
     * Check the consistency of an embedding and fill the agents array.
     *
     * The method checks the consistency of an embedding, i.e. whether
     * it is injective and whether every agent in the domain matches
     * its image.  As a side effect, it populates the `agents` array
     * with agents from the codomain of the embeddings.
     *
     * @param embedding the embedding to check.
     * @param agents the agents array to fill.
     * @return a `(clash, garbage)` pair, where `clash` is `true` if
     *         the embedding is not injective and `garbage` is the
     *         number of component embeddings that contained agents in
     *         their domain that did not match their images.
     */
    protected[Action] def checkConsistency(
      embedding: Embedding[Mixture.Agent], agents: Agents): (Boolean, Int) = {

      mix.clearMarkedAgents(Visited)
      var clash: Boolean = false
      var garbage: Int = 0
      var i: Int = 0
      for (ce <- embedding) {

        // Check embedding consistency
        // val consistent = ce.indices forall { k =>
        for (k <- ce.indices) {

          // Add agent
          val v = ce(k)
          agents(i) = v
          i += 1

          // Check for clashes
          if (v hasMark Visited) {
            clash = true // Agent already in image => clash!
          } else {
            mix.mark(v, Visited)
          }

          // TODO: This should not be neccesary anymore since
          // pruneLifts does it.

          // Check agent consistency, ie check if the domain and
          // image of the embedding have the same neighbours
          // val u = ce.component(k)
          // (u matches v) && (u.indices forall {
          //   j => (u.neighbour(j), v.neighbour(j)) match {
          //     case (None, _) => true
          //     case (Some(w1), Some(w2)) => ce(w1.index) == w2
          //     case _ => false
          //   }
          // })
        }
        // if (!consistent) {
        //   ce.component.removeEmbedding(ce)
        //   garbage += 1
        // }
      }
      (clash, garbage)
    }

    /**
     * Perform the negative and positive updates after applying an
     * action.
     *
     * @param agents the array of agents array operated on by the
     *        action.
     * @param modifiedAgents the agents that have actually been
     *        modified by the action.
     */
    protected def performUpdates(
      agents: Agents,
      modifiedAgents: Iterable[Mixture.Agent]): Unit =
    {
      // -- Negative update --
      //
      // NOTE: We need to eagerly garbage collect embeddings of
      // observables (otherwise their overestimated count will be
      // plotted).
      //
      // FIXME: However, we should be able to do the negative update
      // for LHS embeddings lazily.  But somehow the lazy garbage
      // collection does not work as it should, so we perform
      // negative updates for all modified agents for now.  To be
      // investigated.
      for (v <- modifiedAgents) v.pruneLifts(!postCondition.isEmpty)

      // -- Positive update --
      // var updates = 0

      // pes is a list of representative pairs of partial embeddings
      for ((ci, ae) <- activationMap; pes <- ae) {
        val c = patternComponents(ci)
        val pes2 = pes map { case (u, v) => (u, agents(v)) }
        val ces = ComponentEmbedding.findEmbeddings(pes2)
        for (ce <- ces) {
          if (!postCondition.isEmpty)
            mix.checkpointAddedEmbedding(ce)
          c.addEmbedding(ce)
          // updates += 1
        }
      }

      val sideAffected = mix.markedAgents(SideEffect)

      // We now need to check them against all remaining registered
      // components to make sure we have found every embedding.
      //
      // TODO: Is there a more efficient way to handle side effects?
      //var sideEffects = 0
      val mas2 = mix.markedAgents(SideEffect)
      for (c <- patternComponents) {
        val pes = for (u <- c; v <- sideAffected) yield (u, v)
        val ces = ComponentEmbedding.findEmbeddings(pes)
        for (ce <- ces) {
          if (!postCondition.isEmpty)
            mix.checkpointAddedEmbedding(ce)
          c.addEmbedding(ce)
          //sideEffects += 1
        }
      }
      //println("# updates: " + updates + ", side effects: " + sideEffects)
    }

    /**
     * Find an activation entry for a given component.
     *
     * Given a connected component `component`, check whether this
     * rule activates that component and if yes, return an appropriate
     * activation entry.
     *
     * @param component the [[Patterns#Pattern.Component]] to check.
     */
    private def findActivation(component: Pattern.Component)
        : ActivationEntry = {

      import PartialEmbedding._

      for (rhsComponent <- rhs.components) yield {

        // Use the partial embedding corresponding to this action to
        // generate a reverse map from RHS agents to LHS agents.
        val rhsToLhs = pe.inverse.toMap

        // Find all the partial embedding components from the current
        // RHS component to the target component.
        val pes = findPartialEmbeddings(rhsComponent, component)

        // We are only interested in partial embeddings whose codomain
        // does not have a meet with the LHS.  If such a meet exists,
        // then the action did not produce a new instance (the
        // instance already existed in the LHS), and hence no
        // activation happened.
        val pesFiltered = pes filter { pe =>
          pe exists {
            case (u, v) =>
              !(rhsToLhs isDefinedAt u) || (rhsToLhs(u) meet v).isEmpty
          }
        }

        // Pick a single (representative) pair per partial embedding.
        pesFiltered map { pe =>
          val (u, v) = pe.head
          (v, rhsAgentOffsets(u))
        }
      }
    }
  }

  /** Companion object of the [[Actions#Action]] class. */
  object Action {

    // RHZ: Since this array is indexed by the agent ids of the lhs
    // I find quite confusing the name Agents. Perhaps Injection
    // would be better?
    type Agents = Array[Mixture.Agent]

    sealed abstract class Atom {
      def apply(agents: Agents, mixture: Mixture)
    }

    final case class AgentAddition(a: AgentIndex, state: AgentState,
      siteStates: Seq[SiteState])
        extends Atom {

      def apply(agents: Agents, mixture: Mixture) {
        mix += (state, siteStates)

        // Register the new agent in the agents array
        val u = mix.head
        agents(a) = u
      }
    }

    final case class AgentDeletion(a: AgentIndex)
        extends Atom {

      def apply(agents: Agents, mixture: Mixture) {
        mix -= agents(a)
      }
    }

    final case class LinkAddition(
      a1: AgentIndex, s1: SiteIndex, l1: LinkState,
      a2: AgentIndex, s2: SiteIndex, l2: LinkState)
        extends Atom {

      def apply(agents: Agents, mixture: Mixture) {
        val u1 = agents(a1)
        val u2 = agents(a2)
        mix connect (u1, s1, l1, u2, s2, l2)
      }
    }

    final case class LinkDeletion(a: AgentIndex, s: SiteIndex)
        extends Atom {

      def apply(agents: Agents, mixture: Mixture) {
        val u = agents(a)
        mix disconnect (u, s)
      }
    }

    final case class AgentStateChange(a: AgentIndex, state: AgentState)
        extends Atom {

      def apply(agents: Agents, mixture: Mixture) {
        val u = agents(a)
        mix updateAgentState (u, state)
      }
    }

    final case class SiteStateChange(
      a: AgentIndex, s: SiteIndex, state: SiteState)
        extends Atom {

      def apply(agents: Agents, mixture: Mixture) {
        val u = agents(a)
        mix updateSiteState (u, s, state)
      }
    }


    /**
     * Construct an action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     *
     * @param lhs the left-hand side of this action.
     * @param rhs the right-hand side of this action.
     * @param pe the partial embedding of this action.
     */
    def mkAtoms(lhs: Pattern, rhs: Pattern, pe: PartialEmbedding,
      rhsAgentOffsets: Map[Pattern.Agent, AgentIndex])
        : Seq[Action.Atom] = {

      import SiteGraph.{Link, Undefined, Stub, Wildcard}
      import Pattern.Linked

      def findStateChange[T <: Matchable[T]](ls: T, rs: T): Option[T] =
        if (ls isEquivTo rs) None         // No state change
        else if (rs.isComplete) Some(rs)  // State refinement or change
        else throw new IllegalArgumentException(
          "attempt to change state " + ls + " to incomplete state " +
            rs + " in rule: " + lhs + " -> " + rhs)

      def linkState(u: Pattern.Agent, j: SiteIndex): LinkState =
        u.links(j) match {
          case Linked(_, _, l) => l
          case _ => throw new IllegalArgumentException(
            "expected site " + j + " of agent " + u +
              " to be linked in RHS of rule " + lhs + " -> " + rhs)
        }

      // Compute the offsets of the first agents of each component of
      // the LHS in the agents array passed to an action application.
      val ceOffsets: Array[Int] =
        lhs.components.scanLeft(0) { (i, ce) => i + ce.length }

      @inline def lhsAgentOffset(a: Pattern.Agent) =
        ceOffsets(a.component.index) + a.index

      @inline def rhsAgentOffset(a: Pattern.Agent) =
        rhsAgentOffsets(a)

      @inline def linkDeletion(
        atoms: mutable.Buffer[Atom], u1: Pattern.Agent,
        j1: SiteIndex, u2: Pattern.Agent) {
        val o1 = lhsAgentOffset(u1)
        val o2 = lhsAgentOffset(u2)
        if (o2 <= o1)
          atoms += LinkDeletion(o1, j1)
      }

      @inline def linkAddition(
        atoms: mutable.Buffer[Atom],
        u1: Pattern.Agent, j1: SiteIndex, l1: LinkState,
        u2: Pattern.Agent, j2: SiteIndex) {
        val o1 = rhsAgentOffset(u1)
        val o2 = rhsAgentOffset(u2)
        val l2 = linkState(u2, j2)
        if (o2 <= o1)
          atoms += LinkAddition(o1, j1, l1, o2, j2, l2)
      }

      @inline def checkLinkComplete(l: LinkState) {
        if (!l.isComplete) throw new IllegalArgumentException(
          "attempt to introduce new link with incomplete link state \"" + l +
            "\" in rule: " + lhs + " -> " + rhs)
      }

      val atoms = new mutable.ArrayBuffer[Atom]()

      // Find all agent deletions
      for (lu <- lhs drop pe.length) {
        atoms += AgentDeletion(lhsAgentOffset(lu))
      }

      // Find all agent additions
      val rhsAdditions = rhsAgentOffsets filter {
        case (ru, ro) => ro >= pe.length
      }
      for ((ru, ro) <- rhsAdditions) {
        if (ru.isComplete) atoms += AgentAddition(ro, ru.state, ru.siteStates)
        else throw new IllegalArgumentException(
          "attempt to add incomplete agent " + ru + " in rule: " +
            lhs + " -> " + rhs)
      }

      // Find all agent state changes (in the common context)
      for {
        (lu, ru) <- pe
        s <- findStateChange(lu.state, ru.state)
      } {
        atoms += AgentStateChange(lhsAgentOffset(lu), s)
      }

      // Find all site state changes (in the common context)
      for {
        (lu, ru) <- pe
        j <- lu.indices
        s <- findStateChange(lu.siteStates(j), ru.siteStates(j))
      } {
        atoms += SiteStateChange(lhsAgentOffset(lu), j, s)
      }

      val linkAtoms = new mutable.ArrayBuffer[Atom]()

      // Find all the link changes (in the common context)
      for ((lu, ru) <- pe; j <- lu.indices) {
        (lu.links(j), ru.links(j)) match {
          case (Stub | Wildcard(_, _, _) | Linked(_, _, _), Undefined) =>
            throw new IllegalArgumentException(
              "attempt to undefine site " + j + " of agent " + lu +
              " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Stub) =>
            linkAtoms += LinkDeletion(lhsAgentOffset(lu), j)
          case (Linked(lu2, _, _), Stub) =>
            linkDeletion(linkAtoms, lu, j, lu2)
          case (Undefined | Stub | Linked(_, _, _), Wildcard(_, _, _)) =>
            throw new IllegalArgumentException(
              "attempt to add wildcard link to site " + j + " of agent " +
              lu + " in rule: " + lhs + " -> " + rhs)
          case (Wildcard(la, ls2, ll), Wildcard(ra, rs2, rl)) =>
            // FIXME: Should we allow link state changes in wildcards?
            // RHZ: What is this check for?
            if (!(la matches ra) && (ls2 matches rs2) && (ll matches rl))
              throw new IllegalArgumentException(
                "attempt to modify wildcard link at site " + j +
                " of agent " + lu + " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Linked(ru2, rj2, rl)) => {
            linkAtoms += LinkDeletion(lhsAgentOffset(lu), j)
            checkLinkComplete(rl)
            linkAddition(linkAtoms, ru, j, rl, ru2, rj2)
          }
          case (Stub, Linked(ru2, rj2, rl)) => {
            checkLinkComplete(rl)
            linkAddition(linkAtoms, ru, j, rl, ru2, rj2)
          }
          case (Linked(lu2, lj2, ll), Linked(ru2, rj2, rl)) =>
            // Is there any change in the link?
            // Possibilities: the neighboring site is
            // different or there's a link state change
            if (!((lhsAgentOffset(lu2) == rhsAgentOffset(ru2)) &&
                  (lj2 == rj2) && findStateChange(ll, rl).isEmpty)) {
              linkDeletion(linkAtoms, lu, j, lu2)
              linkAddition(linkAtoms, ru, j, rl, ru2, rj2)
            }
          case _ => {}
        }
      }

      def atomOrd(a: Atom) = a match {
        case _:LinkAddition => 1
        case _:LinkDeletion => 0
      }

      atoms ++= linkAtoms.sortBy(atomOrd)

      // Find all remaining link additions (between newly added agents)
      for {
        (ru, ro) <- rhsAdditions
        j <- ru.indices
      } {
        ru.links(j) match {
          case Undefined =>
            throw new IllegalArgumentException(
              "attempt to add agent " + ru + " with undefined site " + j +
                " in rule: " + lhs + " -> " + rhs)
          case Wildcard(_, _, _) =>
            throw new IllegalArgumentException(
              "attempt to add agent " + ru + " with wildcard link at site " +
                j + " in rule: " + lhs + " -> " + rhs)
          case Linked(ru2, j2, l) => {
            checkLinkComplete(l)
            linkAddition(atoms, ru, j, l, ru2, j2)
          }
          case _ => { }
        }
      }

      atoms.toList
    }
  }
  // End of Action companion object


  /** Convert a pair `(lhs, rhs)` of patterns into an action. */
  implicit def patternPairToAction(lr: (Pattern, Pattern))(
    implicit ab: ActionBuilder): RuleBuilder =
    ab(lr._1, lr._2)


  /** Base class for factory objects used to build actions. */
  abstract class ActionBuilder {

    // FIXME Find a better name for B
    type B <: RuleBuilder

    /**
     * Construct an action from a LHS and RHS pattern.
     *
     * @param lhs the left-hand side of the resulting action.
     * @param rhs the right-hand side of the resulting action.
     */
    def apply(lhs: Pattern, rhs: Pattern): B
  }


  /** A trait for Rule builders. */
  trait RuleBuilder {

    def getAction: Action

    /**
     * Constructor for rules that follow an arbitrary rate law.
     *
     * @param law kinetic law
     */
    def withRateLaw(law: () => Double): Rule
  }


  // --- BiActions ---

  // RHZ: What is rhsAgentOffsets for?
  // Is it redundant to also have lhsAgentOffsets?
  final class BiAction(
    val lhs: Pattern,
    val rhs: Pattern,
    val fwdPe: PartialEmbedding,
    val bwdPe: PartialEmbedding,
    val lhsAgentOffsets: Map[Pattern.Agent, AgentIndex],
    val rhsAgentOffsets: Map[Pattern.Agent, AgentIndex],
    val preCondition:  Option[(Action, Action.Agents) => Boolean],
    val postCondition: Option[(Action, Action.Agents) => Boolean]) {

    @inline def fwdAction = new Action(lhs, rhs, fwdPe,
      rhsAgentOffsets, preCondition, postCondition)

    // TODO: We could use fwdPe.inverse here right?
    @inline def bwdAction = new Action(rhs, lhs, bwdPe,
      lhsAgentOffsets, preCondition, postCondition)
  }


  /** Base class for factory objects used to build actions. */
  abstract class BiActionBuilder {

    // FIXME Find a better name for B
    type B <: BiRuleBuilder

    /**
     * Construct an action from a LHS and RHS pattern.
     *
     * @param lhs the left-hand side of the resulting action.
     * @param rhs the right-hand side of the resulting action.
     */
    def apply(lhs: Pattern, rhs: Pattern): B
  }


  /** A trait for BiRule builders. */
  trait BiRuleBuilder {

    def getBiAction: BiAction

    /**
     * Constructor for rules that follow arbitrary rate laws.
     *
     * @param fwdLaw kinetic law of forward rule
     * @param bwdLaw kinetic law of backward rule
     */
    def withRateLaws(fwdLaw: () => Double, bwdLaw: () => Double): BiRule
  }
}

