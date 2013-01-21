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

    def apply(embedding: Embedding, mixture: Mixture = mix) {

      // Check for null events
      // FIXME: Implement this properly!
      mix.clearUpdatedAgents
      val clash = embedding exists { pe =>
        pe exists { a =>
          if (a.updated) true
          else {
            mix.markUpdated(a)
            false
          }
        }
      }

      if (clash) {
        println("# clash!")
      } else {

        // Allocate an extra partial embedding to hold added agents
        // FIXME: This seems like a hack...
        val addedPeInj = new Array[Mixture.Agent](additions)
        val emb = embedding :+ new PartialEmbedding(addedPeInj, null)

        // Clear the updated agents list of mix
        mix.clearUpdatedAgents

        // Apply all actions
        for (a <- atoms) a(emb, mixture)

        // FIXME: Handle positive/negative update.
      }
    }

    def :@ (rate: => Double) = new Rule(this, ns => ns.product * rate)
    def !@ (law: (Int*) => Double) = new Rule(this, law) // see https:/ / github.com/jkrivine/KaSim/issues/9
  }

  object Action {

    sealed abstract class Atom {//extends Function2[Embedding, Mixture, Unit]
      def apply(embedding: Embedding, mixture: Mixture)
    }

    final case class AgentAddition(c: ComponentIndex, a: AgentIndex,
      state: AgentState, siteStates: Seq[SiteState])
        extends Atom {

      def apply(embedding: Embedding, mixture: Mixture) {
        mix += (state, siteStates)

        // Register new agent in specially allocated partial embedding
        val u = mix.head
        embedding(c).inj(a) = u
      }
    }

    final case class AgentDeletion(c: ComponentIndex, a: AgentIndex)
        extends Atom {

      def apply(embedding: Embedding, mixture: Mixture) {
        mix -= embedding(c)(a)
      }
    }

    final case class LinkAddition(
      c1: ComponentIndex, a1: AgentIndex, s1: SiteIndex, l1: LinkState,
      c2: ComponentIndex, a2: AgentIndex, s2: SiteIndex, l2: LinkState)
        extends Atom {

      def apply(embedding: Embedding, mixture: Mixture) {
        val u1 = embedding(c1)(a1)
        val u2 = embedding(c2)(a2)
        mix connect (u1, s1, l1, u2, s2, l2)
      }
    }

    final case class LinkDeletion(
      c: ComponentIndex, a: AgentIndex, s: SiteIndex) extends Atom {

      def apply(embedding: Embedding, mixture: Mixture) {
        val u = embedding(c)(a)
        mix disconnect (u, s)
      }
    }

    final case class AgentStateChange(
      c: ComponentIndex, a: AgentIndex, state: AgentState)
        extends Atom {

      def apply(embedding: Embedding, mixture: Mixture) {
        val u = embedding(c)(a)
        u.state = state
        mix.markUpdated(u)  // FIXME: Should do this in a method in Mixture.
      }
    }

    final case class SiteStateChange(
      c: ComponentIndex, a: AgentIndex, s: SiteIndex, state: SiteState)
        extends Atom {

      def apply(embedding: Embedding, mixture: Mixture) {
        val u = embedding(c)(a)
        u.sites(s).state = state
        mix.markUpdated(u)  // FIXME: Should do this in a method in Mixture.
      }
    }


    /**
     * Construct an action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     *
     * FIXME: This will only work for versions of Kappa where the
     * agent state is exactly equivalent to the agent type.  A
     * possible way to fix this would be to define a sub-trait of the
     * State trait that represents agent states and provides a
     * "typeMatches" method or similar.  Needs discussion.
     */
    def apply(lhs: Pattern, rhs: Pattern): Action = {

      import State._
      import Pattern._

      def findStateChange[T <: State[T]](ls: T, rs: T): Option[T] =
        if (sequals(ls, rs)) None         // No state change
        else if (rs.isComplete) Some(rs)  // State refinement or change
        else throw new IllegalArgumentException(
          "attempt to change state " + ls + " to incomplete state " +
            rs + " in rule: " + lhs + " -> " + rhs)

      def isComplete(a: Pattern.Agent) =
        a.state.isComplete && (a.sites forall {
          s => s.state.isComplete && (s.link match {
            case Stub => true
            case Linked(_, _, l) => l.isComplete
            case _ => false
          })
        })

      def linkState(a: Agent, s: SiteIndex): LinkState =
        a.sites(s).link match {
          case Linked(_, _, l) => l
          case _ => throw new IllegalArgumentException(
            "expected state " + s + " of agent " + a +
              " to be linked in RHS of rule " + lhs + " -> " + rhs)
        }

      def fixLink(atoms: mutable.Buffer[Atom],
        a1: Agent, i1: AgentIndex, s1: SiteIndex, l1: LinkState,
        a2: Agent, i2: AgentIndex, s2: SiteIndex) {
        if (i2 <= i1)
          atoms += LinkAddition(
            a1.component.index, a1.index, s1, l1,
            a2.component.index, a2.index, s2, linkState(a2, s2))
      }

      val atoms = new mutable.ArrayBuffer[Atom]()

      val zipped = (lhs zip rhs)
      val firstDiffIndex = zipped indexWhere {
        case (la, ra) => !sequals(la.state, ra.state)
      }
      val commonPrefixLength =
        if (firstDiffIndex > 0) firstDiffIndex else zipped.length

      // Find all agent deletions
      for (la <- lhs drop commonPrefixLength) {
        atoms += AgentDeletion(la.component.index, la.index)
      }

      // Find all agent additions
      var additions = 0
      val addedPeIndex = lhs.components.size
      for (i <- commonPrefixLength until rhs.size) {
        val ra = rhs(i)
        if (isComplete(ra)) atoms += AgentAddition(
          addedPeIndex, i - commonPrefixLength, ra.state,
          (for (s <- ra.sites) yield s.state))
        else throw new IllegalArgumentException(
          "attempt to add incomplete agent " + ra + " in rule: " +
            lhs + " -> " + rhs)
        additions += 1
      }

      val longestCommonPrefix = zipped take commonPrefixLength
      val lhsIndex = lhs.zipWithIndex.toMap
      val rhsIndex = rhs.zipWithIndex.toMap

      // There are no agent changes by construction. See comment above.

      // Find all state changes (in the longest common prefix)
      for {
        (la, ra) <- longestCommonPrefix if la.length == ra.length
        i <- 0 until la.length
        s <- findStateChange(la.sites(i).state, ra.sites(i).state)
      } {
        atoms += SiteStateChange(la.component.index, la.index, i, s)
      }

      // Find all the link changes in the longest common prefix
      for {
        i <- 0 until commonPrefixLength if lhs(i).length == rhs(i).length
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
            atoms += LinkDeletion(la.component.index, la.index, j)
          case (Linked(la2, _, _), Stub) =>
            if (lhsIndex(la2) <= i) atoms += LinkDeletion(
              la.component.index, la.index, j)
          case (Undefined | Stub | Linked(_, _, _), Wildcard(_, _, _)) =>
            throw new IllegalArgumentException(
              "attempt to add wildcard link to site " + j + " of agent " +
                la + " in rule: " + lhs + " -> " + rhs)
          case (Wildcard(la2, ls2, ll), Wildcard(ra2, rs2, rl)) =>
            if (!(omatches(la2, ra2)(sequals(_, _)) &&
              omatches(ls2, rs2)(sequals(_, _)) &&
              omatches(ll, rl)(sequals(_, _))))
              throw new IllegalArgumentException(
                "attempt to modify wildcard link at site " + j +
                  " of agent " + la + " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Linked(ra2, rj2, rl)) =>
            atoms += LinkDeletion(la.component.index, la.index, j)
            fixLink(atoms, la, i, j, rl, ra2, rhsIndex(ra2), rj2)
          case (Stub, Linked(ra2, rj2, rl)) =>
            fixLink(atoms, la, i, j, rl, ra2, rhsIndex(ra2), rj2)
          case (Linked(la2, lj2, ll), Linked(ra2, rj2, rl)) => {
            val li = lhsIndex(la2)
            val ri = rhsIndex(ra2)
            if (!((li == ri) && (ls == rs) &&
              findStateChange(ll, rl).isEmpty)) {
              if (li <= i) atoms += LinkDeletion(
                la.component.index, la.index, j)
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
              val (c2, a2) =
                if (ri < commonPrefixLength)
                  (lhs(ri).component.index, lhs(ri).index)
                else
                  (addedPeIndex, ri - commonPrefixLength)
              atoms += LinkAddition(
                addedPeIndex, i - commonPrefixLength, j, l,
                c2, a2, j2, linkState(ra2, j2))
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
