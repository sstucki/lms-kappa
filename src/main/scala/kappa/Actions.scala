package kappa

import scala.language.implicitConversions

import scala.collection.mutable


trait Actions {
  this: LanguageContext
      with SiteGraphs
      with ContactGraphs
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
   *        inj array of the agents in the RHS.
   * @param preCondition an optional predicate to execute ''before''
   *        applying the action.  If `preCondition` is `Some(f)` and
   *        if `f` applied to the inj array returns `false`, the
   *        action will not be applied and the mixture remains
   *        untouched.
   * @param postCondition a predicate to execute ''after'' applying
   *        the action.  If `postCondition` is `Some(f)` and `f`
   *        applied to the inj array returns `false` the action
   *        application will be canceled and the state of the mixture
   *        prior to the action application will be restored.
   */
  final class Action(
    val lhs: Pattern,
    val rhs: Pattern,
    val pe: PartialEmbedding,
    val rhsAgentOffsets: Map[Pattern.Agent, AgentIndex],
    val preCondition:  Option[(Action, Action.Injection) => Boolean] = None,
    val postCondition: Option[(Action, Action.Injection) => Boolean] = None)
      extends Function2[Embedding[Mixture.Agent], Mixture,
        (Boolean, Boolean)]
  {
    import Action._

    private var _atoms: Seq[Action.Atom] = null

    /** The atomic actions making up this action. */
    def atoms: Seq[Action.Atom] =
      if (_atoms == null) register else _atoms

    /** Register this action. */
    def register: Seq[Action.Atom] = {

      // Register the components of LHS pattern
      val componentIndices = lhs.registerComponents

      // Compute the atomic actions
      _atoms = mkAtoms(lhs, rhs, pe, rhsAgentOffsets)

      // Find positive influence of this rule on every registered
      // component and initialize the positive influence map
      // of the action accordingly.
      for (c <- patternComponents) addActivation(c)

      _atoms
    }

    /** An activation entry is a sequence of sequences of pairs (u, v)
      * that represent a partial embedding, where u is the agent in the
      * target pattern component that's activated and v is the index of
      * the agent in the source pattern component.
      *
      * RHZ: Why is it a sequence of sequences? Why not just a seq?
      *
      * TODO: A clearer explanation is needed.
      */
    type ActivationEntry = Iterable[Iterable[(Pattern.Agent, AgentIndex)]]

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

          // RHZ: The first part of the predicate is redundant no?
          // Or does the += above have side effects?
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


    // -- Function2[Embedding, Mixture, (Boolean, Boolean)] API --

    /**
     * Apply this action to a mixture along a given embedding.
     *
     * @param embedding the embedding along which the action should
     *        be applied.
     * @param mix the mixture to which the action should be applied.
     * @return a tuple of two booleans. The first boolean is `true`
     *         if the action application resulted in a productive
     *         event, `false` otherwise.  The second boolean is `true`
     *         if the embedding is not injective.
     */
    def apply(embedding: Embedding[Mixture.Agent],
      mixture: Mixture = mix): (Boolean, Boolean) = {

      if (!checkInjectivity(embedding, mixture)) {
        println("# clash!")
        (false, true)
      } else {

        // Populate the injection array
        val additions = rhs.length - pe.length
        if ((embedding map (_.length)).sum != lhs.length)
          throw new IllegalStateException("assertion failed")
        val inj = new Injection(lhs.length + additions)
        var i = 0
        for (ce <- embedding; v <- ce) {
          inj(i) = v
          i += 1
        }

        if (!(preCondition map { f => f(this, inj) } getOrElse true)) {
          println("# pre-condition = false.")
          (false, false)
        } else {

          // If the post-condition is defined, checkpoint the mixture so
          // we can restore its current state if the post-condition
          // evaluates to `false`.
          if (!postCondition.isEmpty) mixture.checkpoint

          // Clear the marked agents list of mix
          mixture.clearMarkedAgents(Updated)
          mixture.clearMarkedAgents(SideEffect)

          // Apply all atomic actions
          for (a <- atoms) a(inj, mixture)

          // Perform negative and positive update
          val modifiedAgents = mixture.markedAgents(Updated)
          // RHZ: I think this solves the performance issue we were
          // having by marking too many agents as side affected
          val sideAffected = mixture.markedAgents(SideEffect) diff inj
          performUpdates(inj, modifiedAgents, sideAffected)

          // Check post-conditions
          if (postCondition map { f => f(this, inj) } getOrElse true) {
            // Discard pre-application checkpoint and perform
            // negative/positive updates.
            if (!postCondition.isEmpty) mixture.discardCheckpoint
            (true, false)
          } else {
            // Roll back to the state of the mixture prior to the
            // action application.
            println("# post-condition = false.")
            mixture.rollback
            (false, false)
          }
        }
      }
    }


    // -- Any API --
    override def toString: String = atoms.toString


    // -- Protected/private methods --

    /**
     * Check whether an embedding is injective, i.e. whether each
     * agent of each connected component in the domain of the
     * embedding is mapped to a different agent in the mixture.
     *
     * @param embedding the embedding to check.
     * @param mixture the mixture where the agents in the codomain are.
     * @return `true` if the embedding is injective.
     */
    protected[Action] def checkInjectivity(
      embedding: Embedding[Mixture.Agent],
      mixture: Mixture): Boolean = {

      mixture.clearMarkedAgents(Visited)

      embedding forall { ce =>
        ce forall { v =>
          // Check for clashes
          if (v hasMark Visited) {
            false // Agent already in image => clash!
          } else {
            mixture.mark(v, Visited)
            true
          }
        }
      }
    }

    /**
     * Perform the negative and positive updates after applying an
     * action.
     *
     * @param inj the array of agents operated on by the action.
     * @param modifiedAgents the agents that have actually been
     *        modified by the action.
     * @param sideAffected the agents that have been modified
     *        by the action but are not part of the inj array.
     */
    protected def performUpdates(
      inj: Injection,
      modifiedAgents: Iterable[Mixture.Agent],
      sideAffected: Iterable[Mixture.Agent]) {

      // -- Negative update --
      //
      // TODO: We should be able to do the negative update
      // for LHS embeddings lazily.  But somehow the lazy garbage
      // collection does not work as it should, so we perform
      // negative updates for all modified agents for now.
      // To be investigated.
      //
      // NOTE: When the lazy negative update is enabled, we still
      // need to eagerly garbage collect embeddings of observables
      // (otherwise their overestimated count will be plotted).
      for (v <- modifiedAgents ++ sideAffected)
        v.pruneLifts(!postCondition.isEmpty)

      // -- Positive update --
      //
      // pes is a list of representative pairs of partial embeddings
      // (u, v) where u represents the activated connected component
      // and v represents the index of the agent in the inj array.
      for ((ci, ae) <- activationMap; pes <- ae) {
        val c = patternComponents(ci)
        val pesInMix = pes map { case (u, v) => (u, inj(v)) }
        val ces = ComponentEmbedding.findEmbeddings(pesInMix)
        for (ce <- ces) {
          if (!postCondition.isEmpty)
            mix.checkpointAddedEmbedding(ce)
          c.addEmbedding(ce)
        }
      }

      // We now need to check them against all remaining registered
      // components to make sure we have found every embedding.
      //
      // TODO: Is there a more efficient way to handle side effects?
      for (c <- patternComponents) {
        val pes = for (u <- c; v <- sideAffected) yield (u, v)
        val ces = ComponentEmbedding.findEmbeddings(pes)
        for (ce <- ces) {
          if (!postCondition.isEmpty)
            mix.checkpointAddedEmbedding(ce)
          c.addEmbedding(ce)
        }
      }
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

    type Injection = Array[Mixture.Agent]

    sealed abstract class Atom {
      def apply(inj: Injection, mixture: Mixture)
    }

    abstract class AgentAddition[T <: ContactGraph.Agent](
      val a: AgentIndex,
      val agentType: T,
      val state: AgentState)
        extends Atom {

      val siteTypes: Seq[agentType.Site]
      val siteStates: Seq[SiteState]

      def apply(inj: Injection, mixture: Mixture) {
        mixture.addAgent(agentType, state)(siteTypes, siteStates)

        // Register the new agent in the inj array
        val u = mixture.head
        mixture.mark(u, Updated)
        inj(a) = u
      }
    }

    object AgentAddition {
      def apply(a: AgentIndex, agentType: ContactGraph.Agent,
        state: AgentState)(st: Seq[agentType.Site],
          ss: Seq[SiteState]) = {
        new AgentAddition[agentType.type](a, agentType, state) {
          val siteTypes = st
          val siteStates = ss
        }
      }
    }

    final case class AgentDeletion(a: AgentIndex)
        extends Atom {

      def apply(inj: Injection, mixture: Mixture) {
        val agent = inj(a)
        mixture -= agent
        mixture.checkpointAgent(agent)
        mixture.mark(agent, Updated)
        mixture.mark(agent, Deleted)
        for (Mixture.Linked(u, i, _, _) <- agent.links) {
          mixture.checkpointAgent(u)
          mixture.mark(u, SideEffect)
        }
      }
    }

    final case class LinkAddition(
      a1: AgentIndex, s1: SiteIndex, t1: ContactGraph.Link, l1: LinkState,
      a2: AgentIndex, s2: SiteIndex, t2: ContactGraph.Link, l2: LinkState)
        extends Atom {

      def apply(inj: Injection, mixture: Mixture) {
        val u1 = inj(a1)
        val u2 = inj(a2)
        mixture.checkpointAgent(u1)
        mixture.checkpointAgent(u2)
        mixture.connect(u1, s1, t1, l1, u2, s2, t2, l2)
        mixture.mark(u1, Updated)
        mixture.mark(u2, Updated)
      }
    }

    final case class LinkDeletion(a: AgentIndex, s: SiteIndex,
      sideAffected: Boolean)
        extends Atom {

      def apply(inj: Injection, mixture: Mixture) {
        val u = inj(a)
        u.links(s) match {
          case Mixture.Linked(u2, _, _, _) => {
            mixture.checkpointAgent(u2)
            mixture.mark(u2, if (sideAffected) SideEffect else Updated)
          }
          case _ => ()
        }
        mixture.checkpointAgent(u)
        mixture.disconnect(u, s)
        mixture.mark(u, Updated)
      }
    }

    final case class AgentStateChange(a: AgentIndex, state: AgentState)
        extends Atom {

      def apply(inj: Injection, mixture: Mixture) {
        val u = inj(a)
        mixture.checkpointAgent(u)
        u.state = state
        // mixture.updateAgentState(u, state)
        mixture.mark(u, Updated)
      }
    }

    final case class SiteStateChange(
      a: AgentIndex, s: SiteIndex, state: SiteState)
        extends Atom {

      def apply(inj: Injection, mixture: Mixture) {
        val u = inj(a)
        mixture.checkpointAgent(u)
        u.siteStates(s) = state
        // mixture.updateSiteState(u, s, state)
        mixture.mark(u, Updated)
      }
    }

    // RHZ: There's no LinkStateChange??


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

      def linkState(u: Pattern.Agent, j: SiteIndex) =
        u.links(j) match {
          case Linked(_, _, t, l) => (t, l)
          case _ => throw new IllegalArgumentException(
            "expected site " + j + " of agent " + u +
              " to be linked in RHS of rule " + lhs + " -> " + rhs)
        }

      // Compute the offsets of the first agents of each component of
      // the LHS in the inj array passed to an action application.
      val ceOffsets: Array[Int] =
        lhs.components.scanLeft(0) { (i, ce) => i + ce.length }

      @inline def lhsAgentOffset(a: Pattern.Agent) =
        ceOffsets(a.component.index) + a.index

      @inline def rhsAgentOffset(a: Pattern.Agent) =
        rhsAgentOffsets(a)

      @inline def linkDeletion(atoms: mutable.Buffer[Atom],
        u1: Pattern.Agent, j1: SiteIndex,
        u2: Pattern.Agent, sideAffected: Boolean) {
        val o1 = lhsAgentOffset(u1)
        val o2 = lhsAgentOffset(u2)
        if (o2 <= o1)
          atoms += LinkDeletion(o1, j1, sideAffected)
      }

      @inline def linkAddition(atoms: mutable.Buffer[Atom],
        u1: Pattern.Agent, j1: SiteIndex, t1: ContactGraph.Link, l1: LinkState,
        u2: Pattern.Agent, j2: SiteIndex) {
        val o1 = rhsAgentOffset(u1)
        val o2 = rhsAgentOffset(u2)
        val (t2, l2) = linkState(u2, j2)
        if (o2 <= o1)
          atoms += LinkAddition(o1, j1, t1, l1, o2, j2, t2, l2)
      }

      @inline def checkLinkComplete(l: LinkState) {
        if (!l.isComplete) throw new IllegalArgumentException(
          "attempt to introduce new link with incomplete link state" +
            "\"" + l + "\" in rule: " + lhs + " -> " + rhs)
      }

      val atoms = new mutable.ArrayBuffer[Atom]()

      // FIXME: We are using the longest common prefix convention
      // here but we are not supposed to.

      // Find all agent deletions
      for (lu <- lhs drop pe.length) {
        atoms += AgentDeletion(lhsAgentOffset(lu))
      }

      // Find all agent additions
      val rhsAdditions = rhsAgentOffsets filter {
        case (ru, ro) => ro >= pe.length
      }
      for ((ru, ro) <- rhsAdditions) {
        if (ru.isComplete)
          atoms += AgentAddition(ro, ru.agentType, ru.state)(
            ru.siteTypes, ru.siteStates)
        else throw new IllegalArgumentException(
          "attempt to add incomplete agent " + ru + " in rule: " +
            lhs + " -> " + rhs)
      }

      // Find all agent state changes (in the common context)
      for { (lu, ru) <- pe
            s <- findStateChange(lu.state, ru.state)
      } atoms += AgentStateChange(lhsAgentOffset(lu), s)

      // Find all site state changes (in the common context)
      for { (lu, ru) <- pe
            j <- lu.indices
            s <- findStateChange(lu.siteStates(j), ru.siteStates(j))
      } atoms += SiteStateChange(lhsAgentOffset(lu), j, s)

      val linkAtoms = new mutable.ArrayBuffer[Atom]()

      // Find all the link changes (in the common context)
      for ((lu, ru) <- pe; j <- lu.indices) {
        (lu.links(j), ru.links(j)) match {
          case (Linked(lu2, lj2, lt, ll), Linked(ru2, rj2, rt, rl)) => {
            // Is there any change in the link?
            // Possibilities: the neighbouring site is different or
            // there's a link state change
            if (!((lhsAgentOffset(lu2) == rhsAgentOffset(ru2)) &&
                  (lj2 == rj2) && findStateChange(ll, rl).isEmpty)) {
              linkDeletion(linkAtoms, lu, j, lu2, false)
              linkAddition(linkAtoms, ru, j, rt, rl, ru2, rj2)
            }
          }
          case (Stub, Linked(ru2, rj2, rt, rl)) => {
            checkLinkComplete(rl)
            linkAddition(linkAtoms, ru, j, rt, rl, ru2, rj2)
          }
          case (Stub | Wildcard(_, _, _) |
                Linked(_, _, _, _), Undefined) =>
            throw new IllegalArgumentException(
              "attempt to undefine site " + j + " of agent " + lu +
              " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Stub) =>
            linkAtoms += LinkDeletion(lhsAgentOffset(lu), j, true)
          case (Linked(lu2, _, _, _), Stub) =>
            linkDeletion(linkAtoms, lu, j, lu2, false)
          case (Undefined | Stub |
                Linked(_, _, _, _), Wildcard(_, _, _)) =>
            throw new IllegalArgumentException(
              "attempt to add wildcard link to site " + j +
              " of agent " + lu + " in rule: " + lhs + " -> " + rhs)
          case (Wildcard(la, ls2, ll), Wildcard(ra, rs2, rl)) =>
            // TODO: Should we allow link state changes in wildcards?
            if (!((la matches ra) && (ls2 matches rs2) &&
                  (ll matches rl)))
              throw new IllegalArgumentException(
                "attempt to modify wildcard link at site " + j +
                " of agent " + lu + " in rule: " + lhs + " -> " + rhs)
          case (Undefined |
                Wildcard(_, _, _), Linked(ru2, rj2, rt, rl)) => {
            linkAtoms += LinkDeletion(lhsAgentOffset(lu), j, true)
            checkLinkComplete(rl)
            linkAddition(linkAtoms, ru, j, rt, rl, ru2, rj2)
          }
          case _ => ()
        }
      }

      def atomOrd(a: Atom) = a match {
        case _:LinkDeletion => 0
        case _:LinkAddition => 1
      }

      atoms ++= linkAtoms.sortBy(atomOrd)

      // Find all remaining link additions (between newly added agents)
      for { (ru, ro) <- rhsAdditions
            j <- ru.indices
      } ru.links(j) match {
        case Undefined =>
          throw new IllegalArgumentException(
            "attempt to add agent " + ru + " with undefined site " +
              j + " in rule: " + lhs + " -> " + rhs)
        case Wildcard(_, _, _) =>
          throw new IllegalArgumentException(
            "attempt to add agent " + ru + " with wildcard link " +
              "at site " + j + " in rule: " + lhs + " -> " + rhs)
        case Linked(ru2, j2, t, l) => {
          checkLinkComplete(l)
          linkAddition(atoms, ru, j, t, l, ru2, j2)
        }
        case _ => ()
      }

      atoms.toList
    }
  }
  // End of Action companion object


  /** Convert a pair `(lhs, rhs)` of patterns into an action. */
  // implicit def patternPairToAction(lr: (Pattern, Pattern))(
  //   implicit ab: ActionBuilder): ab.RuleBuilder =
  //   ab(lr._1, lr._2)


  /** Base class for factory objects used to build actions. */
  abstract class ActionBuilder {

    type RuleBuilder <: RuleBuilderIntf

    /**
     * Construct an action from a LHS and RHS pattern.
     *
     * @param lhs the left-hand side of the resulting action.
     * @param rhs the right-hand side of the resulting action.
     */
    def apply(lhs: Pattern, rhs: Pattern): RuleBuilder
  }


  /** A trait for Rule builders. */
  trait RuleBuilderIntf {

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
    val preCondition:  Option[(Action, Action.Injection) => Boolean],
    val postCondition: Option[(Action, Action.Injection) => Boolean]) {

    @inline def fwdAction = new Action(lhs, rhs, fwdPe,
      rhsAgentOffsets, preCondition, postCondition)

    // TODO: We could use fwdPe.inverse here right?
    @inline def bwdAction = new Action(rhs, lhs, bwdPe,
      lhsAgentOffsets, preCondition, postCondition)

    if (bwdPe != fwdPe.inverse)
      throw new IllegalArgumentException("assertion failed: " +
        "bwdPe != fwdPe.inverse")
  }


  /** Base class for factory objects used to build actions. */
  abstract class BiActionBuilder {

    type BiRuleBuilder <: BiRuleBuilderIntf

    /**
     * Construct an action from a LHS and RHS pattern.
     *
     * @param lhs the left-hand side of the resulting action.
     * @param rhs the right-hand side of the resulting action.
     */
    def apply(lhs: Pattern, rhs: Pattern): BiRuleBuilder
  }


  /** A trait for BiRule builders. */
  trait BiRuleBuilderIntf {

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

