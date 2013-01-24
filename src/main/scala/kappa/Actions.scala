package kappa

import scala.collection.mutable

trait Actions {
  this: LanguageContext with Patterns with Mixtures with Embeddings
      with Rules =>

  // Actions
  final class Action(
    val lhs: Pattern, val rhs: Pattern, val atoms: Seq[Action.Atom],
    val additions: Int)
      extends Function2[Embedding, Mixture, Unit] {

    import Action._

    def apply(embedding: Embedding, mixture: Mixture = mix) {

      // Copy the agents array, check consistency of the embedding and
      // detect clashes
      mix.clearMarkedAgents
      var clash: Boolean = false
      var garbage: Int = 0
      var i: Int = 0
      val totalAgents = (embedding map (_.length)).sum + additions
      val agents = new Array[Mixture.Agent](totalAgents)
      for ((pe, j) <- embedding.zipWithIndex; (v, k) <- pe.zipWithIndex) {
        agents(i) = v; i += 1

        val u = pe.component(k)
        val consistent = (u matches v) && ((0 until u.length) forall {
          s => (u.neighbour(s), v.neighbour(s)) match {
            case (None, _) => true
            case (Some((w1, _)), Some((w2, _))) => true
            case _ => false
          }
        })
        if (!consistent) {
          // FIXME: remove garbage "pe" from PE list of "c".
          garbage += 1
        }

        if (v.marked) {
          clash = true // Agent already in image => clash!
        } else {
          mix.mark(v)
        }
      }

      if (clash) {
        println("# clash!")
      } else if (garbage > 0) {
        println("# found and collected " + garbage + " garbage component embeddings.")
      } else {

        // Clear the updated agents list of mix
        mix.clearMarkedAgents

        // Apply all actions
        for (a <- atoms) a(agents, mixture)

        // FIXME: Handle positive update.
      }
    }

    def :@ (rate: => Double) = new Rule(this, ns => ns.product * rate)
    def !@ (law: (Int*) => Double) = new Rule(this, law) // see https:/ / github.com/jkrivine/KaSim/issues/9
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

      val peOffsets: Vector[Int] =
        lhs.components.scanLeft(0) { (i, pe) => i + pe.length }

      def findStateChange[T <: Matchable[T]](ls: T, rs: T): Option[T] =
        if (ls isEquivTo rs) None         // No state change
        else if (rs.isComplete) Some(rs)  // State refinement or change
        else throw new IllegalArgumentException(
          "attempt to change state " + ls + " to incomplete state " +
            rs + " in rule: " + lhs + " -> " + rhs)

      def linkState(a: Agent, s: SiteIndex): LinkState =
        a.sites(s).link match {
          case Linked(_, _, l) => l
          case _ => throw new IllegalArgumentException(
            "expected state " + s + " of agent " + a +
              " to be linked in RHS of rule " + lhs + " -> " + rhs)
        }

      @inline def fixLink(
        atoms: mutable.Buffer[Atom],
        a1: Agent, i1: AgentIndex, s1: SiteIndex, l1: LinkState,
        a2: Agent, i2: AgentIndex, s2: SiteIndex) {
        if (i2 <= i1)
          atoms += LinkAddition(
            agentOffset(a1), s1, l1,
            agentOffset(a2), s2, linkState(a2, s2))
      }

      @inline def agentOffset(a: Agent) =
        peOffsets(a.component.index) + a.index

      val atoms = new mutable.ArrayBuffer[Atom]()

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

      @inline def addedAgentOffset(i: AgentIndex) =
        peOffsets(lhs.length) + i - commonPrefixLength

      // Find all agent deletions
      for (la <- lhs drop commonPrefixLength) {
        atoms += AgentDeletion(agentOffset(la))
      }

      // Find all agent additions
      var additions = 0
      for (i <- commonPrefixLength until rhs.size) {
        val ra = rhs(i)
        if (ra.isComplete) atoms += AgentAddition(
          addedAgentOffset(i), ra.state,
          (for (s <- ra.sites) yield s.state))
        else throw new IllegalArgumentException(
          "attempt to add incomplete agent " + ra + " in rule: " +
            lhs + " -> " + rhs)
        additions += 1
      }

      val longestCommonPrefix = zipped take commonPrefixLength
      val lhsIndex = lhs.zipWithIndex.toMap
      val rhsIndex = rhs.zipWithIndex.toMap

      // Find all agent state changes (in the longest common prefix)
      for {
        (la, ra) <- longestCommonPrefix
        s <- findStateChange(la.state, ra.state)
      } {
        atoms += AgentStateChange(agentOffset(la), s)
      }


      // Find all site state changes (in the longest common prefix)
      for {
        (la, ra) <- longestCommonPrefix
        i <- 0 until la.length
        s <- findStateChange(la.sites(i).state, ra.sites(i).state)
      } {
        atoms += SiteStateChange(agentOffset(la), i, s)
      }

      // Find all the link changes in the longest common prefix
      for {
        i <- 0 until commonPrefixLength
        j <- 0 until lhs(i).length
      } {
        val la = lhs(i)
        val ra = rhs(i)
        val ls = la(j)
        val rs = ra(j)
        (ls.link, rs.link) match {
          case (Stub | Wildcard(_, _, _) | Linked(_, _, _) , Undefined) =>
            throw new IllegalArgumentException(
              "attempt to undefine site " + j + " of agent " + la +
                " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Stub) =>
            atoms += LinkDeletion(agentOffset(la), j)
          case (Linked(la2, _, _), Stub) =>
            if (lhsIndex(la2) <= i) atoms += LinkDeletion(agentOffset(la), j)
          case (Undefined | Stub | Linked(_, _, _), Wildcard(_, _, _)) =>
            throw new IllegalArgumentException(
              "attempt to add wildcard link to site " + j + " of agent " +
                la + " in rule: " + lhs + " -> " + rhs)
          case (Wildcard(la2, ls2, ll), Wildcard(ra2, rs2, rl)) =>
            if (!(la2 matches ra2) && (ls2 matches rs2) && (ll matches rl))
              throw new IllegalArgumentException(
                "attempt to modify wildcard link at site " + j +
                  " of agent " + la + " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Linked(ra2, rj2, rl)) =>
            atoms += LinkDeletion(agentOffset(la), j)
            fixLink(atoms, la, i, j, rl, ra2, rhsIndex(ra2), rj2)
          case (Stub, Linked(ra2, rj2, rl)) =>
            fixLink(atoms, la, i, j, rl, ra2, rhsIndex(ra2), rj2)
          case (Linked(la2, lj2, ll), Linked(ra2, rj2, rl)) => {
            val li = lhsIndex(la2)
            val ri = rhsIndex(ra2)
            if (!((li == ri) && (ls == rs) &&
              findStateChange(ll, rl).isEmpty)) {
              if (li <= i) atoms += LinkDeletion(agentOffset(la), j)
              fixLink(atoms, la, i, j, rl, ra2, ri, rj2)
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
        val ra = rhs(i)
        val rs = ra(j)
        rs.link match {
          case Undefined =>
            throw new IllegalArgumentException(
              "attempt to add agent " + ra + " with undefined site " + j +
                " in rule: " + lhs + " -> " + rhs)
          case Wildcard(_, _, _) =>
            throw new IllegalArgumentException(
              "attempt to add agent " + ra + " with wildcard link at site " +
                j + " in rule: " + lhs + " -> " + rhs)
          case Linked(ra2, j2, l) => {
            val ri = rhsIndex(ra2)
            if (ri <= i) {
              val a2 =
                if (ri < commonPrefixLength) agentOffset(lhs(ri))
                else addedAgentOffset(ri)
              atoms += LinkAddition(
                addedAgentOffset(i), j, l, a2, j2, linkState(ra2, j2))
            }
          }
          case _ => { }
        }
      }

      // Construct action
      new Action(lhs, rhs, atoms.toList, additions)
    }
  }
}
