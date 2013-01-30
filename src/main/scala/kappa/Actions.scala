package kappa

import scala.collection.mutable

trait Actions {
  this: LanguageContext with Patterns with Mixtures with Embeddings
      with PartialEmbeddings with Rules =>

  // Actions
  final class Action(
    val lhs: Pattern, val rhs: Pattern, val atoms: Seq[Action.Atom],
    val additions: Int,
    // val lhsAgentsModified: Array[Map[AgentIndex, AgentIndex]],
    // val rhsAgentsModified: Array[Map[AgentIndex, AgentIndex]],
    val pe: PartialEmbedding,
    val rhsAgentOffsets: Map[Pattern.Agent, AgentIndex])
      extends Function2[Embedding, Mixture, Boolean] {

    import Action._

    type ActivationEntry = Iterable[Iterable[(Pattern.Agent, AgentIndex)]]

    /** Activation map. */
    val activationMap =
      mutable.HashMap[ComponentIndex, ActivationEntry]()

    /**
     * Apply this action to a mixture along a given embedding.
     *
     * @param embedding the embedding along which the action should
     *        be applied.
     * @param mix the mixture to which the action should be applied.
     * @return `true` if the action application resulted in a
     *         productive event, `false` otherwise.
     */
    def apply(embedding: Embedding, mixture: Mixture = mix): Boolean = {

      // Copy the agents array, check consistency of the embedding and
      // detect clashes
      mix.clearMarkedAgents
      var clash: Boolean = false
      var garbage: Int = 0
      var i: Int = 0
      val totalAgents = (embedding map (_.length)).sum + additions
      val agents = new Array[Mixture.Agent](totalAgents)
      for (ce <- embedding) {

        // Check embedding consistency
        val consistent = (0 until ce.length) forall { k =>

          // Add agent
          val v = ce(k)
          agents(i) = v; i += 1

          // Check for clashes
          if (v.marked) {
            clash = true // Agent already in image => clash!
          } else {
            mix.mark(v)
          }

          // Check agent consistency
          val u = ce.component(k)
          (u matches v) && ((0 until u.length) forall {
            j => (u.neighbour(j), v.neighbour(j)) match {
              case (None, _) => true
              case (Some((w1, _)), Some((w2, _))) => ce(w1.index) == w2
              case _ => false
            }
          })
        }
        if (!consistent) {
          ce.component.removeEmbedding(ce)
          garbage += 1
        }
      }

      if (garbage > 0) {
        //println(
        throw new IllegalStateException(
          "# found and collected " + garbage +
          " garbage component embeddings.")
        //false
      } else if (clash) {
        println("# clash!")
        false
      } else {

        // Clear the marked agents list of mix
        mix.clearMarkedAgents

        // Apply all atomic actions
        for (a <- atoms) a(agents, mixture)

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
        val mas = mix.markedAgents
        for (v <- mas) v.pruneLifts

        // -- Positive update --
        //var updates = 0
        for ((ci, ae) <- activationMap; ps <- ae) {
          val c = patternComponents(ci)
          val ps2 = ps map { case (u, v) => (u, agents(v)) }
          val ces = ComponentEmbedding(ps2)
          for (ce <- ces) {
            c.addEmbedding(ce)
            for (u <- ce) mix.unmark(u)
            //updates += 1
          }
        }

        // All the agents in the codomains of the embeddings we just
        // created using the activation map are now unmarked.  Hence,
        // the only agents left marked are those that were modified by
        // side effects.  We now need to check them against all
        // remaining registered components to make sure we have found
        // every embedding.
        //
        // TODO: Is there a more efficient way to handle side effects?
        //var sideEffects = 0
        val mas2 = mix.markedAgents
        for (c <- patternComponents) {
          val ps = for (u <- c; v <- mas2) yield (u, v)
          val ces = ComponentEmbedding(ps)
          for (ce <- ces) {
            c.addEmbedding(ce)
            //sideEffects += 1
          }
        }
        //println("# updates: " + updates + ", side effects: " + sideEffects)

        true
      }
    }

    // see https:/ / github.com/jkrivine/KaSim/issues/9
    def :@ (rate: => Double) = new Rule(this, () => lhs.count * rate)
    def !@ (law: => Double) = new Rule(this, () => law)

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

    def addActivation(component: Pattern.Component) {
      val idx = component.modelIndex
      if (!(activationMap isDefinedAt idx)) {
        val activations = findActivation(component)
        if (!activations.isEmpty) {
          activationMap += ((idx, activations))

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
  }

  object Action {

    type Agents = Array[Mixture.Agent]

    sealed abstract class Atom {//extends Function2[Embedding, Mixture, Unit]
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
        u.state = state
        mix.mark(u)  // FIXME: Should do this in a method in Mixture.
      }
    }

    final case class SiteStateChange(
      a: AgentIndex, s: SiteIndex, state: SiteState)
        extends Atom {

      def apply(agents: Agents, mixture: Mixture) {
        val u = agents(a)
        u.sites(s).state = state
        mix.mark(u)  // FIXME: Should do this in a method in Mixture.
      }
    }


    /**
     * Construct an action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     */
    def apply(lhs: Pattern, rhs: Pattern): Action = {

      import Pattern._

      def findStateChange[T <: Matchable[T]](ls: T, rs: T): Option[T] =
        if (ls isEquivTo rs) None         // No state change
        else if (rs.isComplete) Some(rs)  // State refinement or change
        else throw new IllegalArgumentException(
          "attempt to change state " + ls + " to incomplete state " +
            rs + " in rule: " + lhs + " -> " + rhs)

      def linkState(u: Agent, s: SiteIndex): LinkState =
        u.sites(s).link match {
          case Linked(_, _, l) => l
          case _ => throw new IllegalArgumentException(
            "expected site " + s + " of agent " + u +
              " to be linked in RHS of rule " + lhs + " -> " + rhs)
        }

      // Compute the offsets of the first agents of each component of
      // the LHS in the agents array passed to an action application.
      val ceOffsets: Vector[Int] =
        lhs.components.scanLeft(0) { (i, ce) => i + ce.length }

      // Find the longest common prefix, i.e. longest prefix in the
      // sequences of agents making up the LHS and RHS, for which the
      // number of sites and the agent states match up.
      val zipped = (lhs zip rhs)
      val firstDiffIndex = zipped indexWhere {
        case (la, ra) =>
          !((la.length == ra.length) &&
            (la.state matchesInLongestCommonPrefix ra.state))
      }
      val commonPrefixLength =
        if (firstDiffIndex > 0) firstDiffIndex else zipped.length

      // Construct the partial embedding corresponding to this action
      val pe = PartialEmbedding(
        lhs take commonPrefixLength, rhs take commonPrefixLength)

      @inline def lhsAgentOffset(a: Agent) =
        ceOffsets(a.component.index) + a.index

      // Compute the RHS agent offsets
      val rhsPrefixAgentOffsets =
        for (i <- 0 until commonPrefixLength)
        yield (rhs(i), lhsAgentOffset(lhs(i)))
      val rhsSuffixAgentOffsets =
        for (i <- commonPrefixLength until rhs.length)
        yield (rhs(i), lhs.length + i - commonPrefixLength)
      val rhsAgentOffsets =
        (rhsPrefixAgentOffsets ++ rhsSuffixAgentOffsets).toMap

      @inline def rhsAgentOffset(a: Agent) =
        rhsAgentOffsets(a)

      @inline def linkDeletion(
        atoms: mutable.Buffer[Atom], u1: Agent, j1: SiteIndex, u2: Agent) {
        val o1 = lhsAgentOffset(u1)
        val o2 = lhsAgentOffset(u2)
        if (o2 <= o1) {
          atoms += LinkDeletion(o1, j1)
        }
      }

      @inline def linkAddition(
        atoms: mutable.Buffer[Atom],
        u1: Agent, s1: SiteIndex, l1: LinkState,
        u2: Agent, s2: SiteIndex) {
        val o1 = rhsAgentOffset(u1)
        val o2 = rhsAgentOffset(u2)
        if (o2 <= o1)
          atoms += LinkAddition(o1, s1, l1, o2, s2, linkState(u2, s2))
      }

      val atoms = new mutable.ArrayBuffer[Atom]()

      // @inline def addedAgentOffset(i: AgentIndex) =
      //   lhs.length + i - commonPrefixLength

      // val lhsAgentsModified = (
      //   for (_ <- 0 until lhs.components.length)
      //   yield Map[AgentIndex, AgentIndex]()).toArray
      // val rhsAgentsModified = (
      //   for (_ <- 0 until rhs.components.length)
      //   yield Map[AgentIndex, AgentIndex]()).toArray

      // @inline def addLhsMod(la: Agent) =
      //   lhsAgentsModified(la.component.index) += ((la.index, agentOffset(la)))

      // @inline def addRhsMod(ri: AgentIndex) = {
      //   val ra = rhs(ri)
      //   rhsAgentsModified(ra.component.index) += (
      //     (ra.index, addedAgentOffset(ri)))
      // }

      // @inline def addLhsRhsMod(la: Agent, ra: Agent) = {
      //   val offs = agentOffset(la)
      //   lhsAgentsModified(la.component.index) += ((la.index, offs))
      //   rhsAgentsModified(ra.component.index) += ((ra.index, offs))
      // }

      // Find all agent deletions
      for (la <- lhs drop commonPrefixLength) {
        atoms += AgentDeletion(lhsAgentOffset(la))
      }

      // Find all agent additions
      val additions = rhs.size - commonPrefixLength
      for (i <- commonPrefixLength until rhs.size) {
        val ra = rhs(i)
        if (ra.isComplete) atoms += AgentAddition(
          lhs.length + i - commonPrefixLength, ra.state,
          (for (s <- ra.sites) yield s.state))
        else throw new IllegalArgumentException(
          "attempt to add incomplete agent " + ra + " in rule: " +
            lhs + " -> " + rhs)
      }

      val longestCommonPrefix = zipped take commonPrefixLength

      // Find all agent state changes (in the longest common prefix)
      for {
        (lu, ru) <- longestCommonPrefix
        s <- findStateChange(lu.state, ru.state)
      } {
        atoms += AgentStateChange(lhsAgentOffset(lu), s)
      }

      // Find all site state changes (in the longest common prefix)
      for {
        (lu, ru) <- longestCommonPrefix
        i <- 0 until lu.length
        s <- findStateChange(lu.sites(i).state, ru.sites(i).state)
      } {
        atoms += SiteStateChange(lhsAgentOffset(lu), i, s)
      }

      // Find all the link changes in the longest common prefix
      for {
        i <- 0 until commonPrefixLength
        j <- 0 until lhs(i).length
      } {
        val lu = lhs(i)
        val ru = rhs(i)
        val ls = lu(j)
        val rs = ru(j)
        (ls.link, rs.link) match {
          case (Stub | Wildcard(_, _, _) | Linked(_, _, _), Undefined) =>
            throw new IllegalArgumentException(
              "attempt to undefine site " + j + " of agent " + lu +
                " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Stub) => {
            atoms += LinkDeletion(lhsAgentOffset(lu), j)
          }
          case (Linked(lu2, _, _), Stub) =>
            linkDeletion(atoms, lu, j, lu2)
          case (Undefined | Stub | Linked(_, _, _), Wildcard(_, _, _)) =>
            throw new IllegalArgumentException(
              "attempt to add wildcard link to site " + j + " of agent " +
                lu + " in rule: " + lhs + " -> " + rhs)
          case (Wildcard(la, ls2, ll), Wildcard(ra, rs2, rl)) =>
            // FIXME: Should we allow link state changes in wildcards?
            if (!(la matches ra) && (ls2 matches rs2) && (ll matches rl))
              throw new IllegalArgumentException(
                "attempt to modify wildcard link at site " + j +
                  " of agent " + lu + " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Linked(ru2, rj2, rl)) => {
            atoms += LinkDeletion(lhsAgentOffset(lu), j)
            linkAddition(atoms, ru, j, rl, ru2, rj2)
          }
          case (Stub, Linked(ru2, rj2, rl)) => {
            linkAddition(atoms, ru, j, rl, ru2, rj2)
          }
          case (Linked(lu2, lj2, ll), Linked(ru2, rj2, rl)) => {
            if (!((lhsAgentOffset(lu2) == rhsAgentOffset(ru2)) &&
              (ls == rs) && findStateChange(ll, rl).isEmpty)) {
              linkDeletion(atoms, lu, j, lu2)
              linkAddition(atoms, ru, j, rl, ru2, rj2)
            }
          }
          case _ => {}
        }
      }

      // Find all remaining link additions (between newly added agents)
      for {
        i <- commonPrefixLength until rhs.length
        j <- 0 until rhs(i).length
      } {
        val ru = rhs(i)
        val rs = ru(j)
        rs.link match {
          case Undefined =>
            throw new IllegalArgumentException(
              "attempt to add agent " + ru + " with undefined site " + j +
                " in rule: " + lhs + " -> " + rhs)
          case Wildcard(_, _, _) =>
            throw new IllegalArgumentException(
              "attempt to add agent " + ru + " with wildcard link at site " +
                j + " in rule: " + lhs + " -> " + rhs)
          case Linked(ru2, j2, l) => {
            linkAddition(atoms, ru, j, l, ru2, j2)
          }
          case _ => { }
        }
      }

      // Construct action
      new Action(lhs, rhs, atoms.toList, additions, pe, rhsAgentOffsets)
    }
  }
}
