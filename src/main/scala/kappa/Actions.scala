package kappa

import scala.collection.mutable

trait Actions {
  this: LanguageContext with Patterns with Mixtures with Embeddings
      with Rules =>

  // Actions
  final class Action(
    val lhs: Pattern, val rhs: Pattern, val atoms: Seq[Action.Atom],
    val additions: Int,
    val lhsAgentsModified: Array[Map[AgentIndex, AgentIndex]],
    val rhsAgentsModified: Array[Map[AgentIndex, AgentIndex]])
      extends Function2[Embedding, Mixture, Unit] {

    import Action._

    type InfluenceEntry = Iterable[Iterable[(AgentIndex, AgentIndex)]]
    /** Positive influence map. */
    val positiveInfluenceMap =
      mutable.HashMap[ComponentIndex, InfluenceEntry]()

    /** Apply this action to a mixture along a given embedding. */
    def apply(embedding: Embedding, mixture: Mixture = mix) {

      // Copy the agents array, check consistency of the embedding and
      // detect clashes
      mix.clearMarkedAgents
      var clash: Boolean = false
      var garbage: Int = 0
      var i: Int = 0
      val totalAgents = (embedding map (_.length)).sum + additions
      val agents = new Array[Mixture.Agent](totalAgents)
      for ((ce, j) <- embedding.zipWithIndex; (v, k) <- ce.zipWithIndex) {
        agents(i) = v; i += 1

        val u = ce.component(k)
        val consistent = (u matches v) && ((0 until u.length) forall {
          s => (u.neighbour(s), v.neighbour(s)) match {
            case (None, _) => true
            case (Some((w1, _)), Some((w2, _))) => ce(w1.index) == w2
            case _ => false
          }
        })
        if (!consistent) {
          ce.component.removeEmbedding(ce)
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
        println("# found and collected " + garbage +
          " garbage component embeddings.")
      } else {

        // Clear the marked agents list of mix
        mix.clearMarkedAgents

        // Apply all actions
        for (a <- atoms) a(agents, mixture)

        // Positive update
        for ((ci, ie) <- positiveInfluenceMap; ps <- ie) {
          val comp = patternComponents(ci)
          val ps2 = ps map { case (sa, ta) => (comp(sa), agents(ta)) }
          val ces = ComponentEmbedding(ps2)
          for (ce <- ces) comp.addEmbedding(ce)
        }

        // FIXME: Handle side effects.
      }
    }

    def :@ (rate: => Double) = new Rule(this, ns => ns.product * rate)
    def !@ (law: (Int*) => Double) = new Rule(this, law) // see https:/ / github.com/jkrivine/KaSim/issues/9

    /**
     * Extend a partial map between agents corresponding represented
     * as an array of [[Pattern.Agent]]s through a traversal of the
     * respective site graphs.
     *
     * @param u the next agent in the pre-image of the map to inspect
     *        in the traversal.
     * @param v the next agent in the image of the map to inspect in
     *        the traversal.
     * @param m the partial map to extend.
     * @param conflicts a conflict map used to avoid the construction
     *        of redundant maps by recording pairs encountered during
     *        previous traversals.
     * @returns `true` if no conflict occurred during expansion.
     */
    private def extendGlueing(
      u: Pattern.Agent, v: Pattern.Agent, m: Array[Pattern.Agent],
      conflicts: Array[mutable.HashSet[Pattern.Agent]]): Boolean = {
      val i = u.index
      if (m(i) != null) true
      else if (conflicts(i) contains v) false
      else {
        if ((u matches v) || (v matches u)) {
          m(i) = v
          conflicts(i) += v
          (0 until u.sites.size) forall { j =>
            (u.neighbour(j), v.neighbour(j)) match {
              case (Some((w1, _)), Some((w2, _))) =>
                extendGlueing(w1, w2, m, conflicts)
              case _ => true
            }
          }
        } else true
      }
    }

    /**
     * Return a sequence of glueings between two pattern components.
     *
     * The partial maps between the agent sets of the two components
     * are represented as maps from agent indices to
     * agent indices.
     *
     * Please note the correct spelling of "glueings" ;-)
     */
    def findGlueings(c1: Pattern.Component, c2: Pattern.Component)
        : Iterable[Map[AgentIndex, AgentIndex]] = {

      val conflicts =
        new Array[mutable.HashSet[Pattern.Agent]](c1.length)
      for (i <- 0 until conflicts.size) {
        conflicts(i) = new mutable.HashSet()
      }
      (for (u <- c1; v <- c2) yield {
        val m = new Array[Pattern.Agent](c1.length)
        if (extendGlueing(u, v, m, conflicts)) {
          val glueing =
            for (i <- (0 until c1.length) if m(i) != null)
            yield (i, m(i).index)
          Some(glueing.toMap)
        } else None
      }).flatten
    }

    private def findPositiveInfluence(component: Pattern.Component)
        : Iterable[Iterable[(AgentIndex, AgentIndex)]] = {

      import scala.util.Sorting

      for (rhsComponent <- rhs.components) yield {

        // Find all the glueings
        val glueings = findGlueings(component, rhsComponent)

        // Compose the glueings with the map from indices of modified
        // agents in the RHS to indices in the action agent array.
        // Retain only non-empty compositions, i.e. those resulting
        // from glueings involving agents that are actually modified
        // by this rule.
        val rcamMods = rhsAgentsModified(rhsComponent.index)
        val comps = glueings map { g =>
          g collect {
            case (k, v) if rcamMods isDefinedAt v => (k, rcamMods(v))
          }
        } filter (!_.isEmpty)

        // Pick a single (representative) pair per glueing.
        comps map (_.head)
      }
    }

    def addPositiveInfluence(component: Pattern.Component) {
      val idx = component.modelIndex
      if (!(positiveInfluenceMap isDefinedAt idx)) {
        val influence = findPositiveInfluence(component)
        if (!influence.isEmpty) {
          positiveInfluenceMap += ((idx, influence))
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

      val ceOffsets: Vector[Int] =
        lhs.components.scanLeft(0) { (i, ce) => i + ce.length }
      val lhsIndex = lhs.zipWithIndex.toMap
      val rhsIndex = rhs.zipWithIndex.toMap

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

      @inline def agentOffset(a: Agent) =
        ceOffsets(a.component.index) + a.index

      @inline def fixLink(
        atoms: mutable.Buffer[Atom],
        a1: Agent, i1: AgentIndex, s1: SiteIndex, l1: LinkState,
        a2: Agent, i2: AgentIndex, s2: SiteIndex) {
        if (i2 <= i1)
          atoms += LinkAddition(
            agentOffset(a1), s1, l1,
            agentOffset(a2), s2, linkState(a2, s2))
      }

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
        ceOffsets(lhs.length) + i - commonPrefixLength

      val lhsAgentsModified = (
        for (_ <- 0 until lhs.components.length)
        yield Map[AgentIndex, AgentIndex]()).toArray
      val rhsAgentsModified = (
        for (_ <- 0 until rhs.components.length)
        yield Map[AgentIndex, AgentIndex]()).toArray

      @inline def addLhsMod(la: Agent) =
        lhsAgentsModified(la.component.index) += ((la.index, agentOffset(la)))

      @inline def addRhsMod(ri: AgentIndex) = {
        val ra = rhs(ri)
        rhsAgentsModified(ra.component.index) += (
          (ra.index, addedAgentOffset(ri)))
      }

      @inline def addLhsRhsMod(la: Agent, ra: Agent) = {
        val offs = agentOffset(la)
        lhsAgentsModified(la.component.index) += ((la.index, offs))
        rhsAgentsModified(ra.component.index) += ((ra.index, offs))
      }

      // Find all agent deletions
      for (la <- lhs drop commonPrefixLength) {
        atoms += AgentDeletion(agentOffset(la))
        addLhsMod(la)
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
        addRhsMod(i)
        additions += 1
      }

      val longestCommonPrefix = zipped take commonPrefixLength

      // Find all agent state changes (in the longest common prefix)
      for {
        (la, ra) <- longestCommonPrefix
        s <- findStateChange(la.state, ra.state)
      } {
        atoms += AgentStateChange(agentOffset(la), s)
        addLhsRhsMod(la, ra)
      }

      // Find all site state changes (in the longest common prefix)
      for {
        (la, ra) <- longestCommonPrefix
        i <- 0 until la.length
        s <- findStateChange(la.sites(i).state, ra.sites(i).state)
      } {
        atoms += SiteStateChange(agentOffset(la), i, s)
        addLhsRhsMod(la, ra)
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
          case (Stub | Wildcard(_, _, _) | Linked(_, _, _), Undefined) =>
            throw new IllegalArgumentException(
              "attempt to undefine site " + j + " of agent " + la +
                " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Stub) => {
            atoms += LinkDeletion(agentOffset(la), j)
            addLhsRhsMod(la, ra)
          }
          case (Linked(la2, _, _), Stub) =>
            if (lhsIndex(la2) <= i) {
              atoms += LinkDeletion(agentOffset(la), j)
              addLhsRhsMod(la, ra)
            }
          case (Undefined | Stub | Linked(_, _, _), Wildcard(_, _, _)) =>
            throw new IllegalArgumentException(
              "attempt to add wildcard link to site " + j + " of agent " +
                la + " in rule: " + lhs + " -> " + rhs)
          case (Wildcard(la2, ls2, ll), Wildcard(ra2, rs2, rl)) =>
            if (!(la2 matches ra2) && (ls2 matches rs2) && (ll matches rl))
              throw new IllegalArgumentException(
                "attempt to modify wildcard link at site " + j +
                  " of agent " + la + " in rule: " + lhs + " -> " + rhs)
          case (Undefined | Wildcard(_, _, _), Linked(ra2, rj2, rl)) => {
            atoms += LinkDeletion(agentOffset(la), j)
            fixLink(atoms, la, i, j, rl, ra2, rhsIndex(ra2), rj2)
            addLhsRhsMod(la, ra)
          }
          case (Stub, Linked(ra2, rj2, rl)) => {
            fixLink(atoms, la, i, j, rl, ra2, rhsIndex(ra2), rj2)
            addLhsRhsMod(la, ra)
          }
          case (Linked(la2, lj2, ll), Linked(ra2, rj2, rl)) => {
            val li = lhsIndex(la2)
            val ri = rhsIndex(ra2)
            if (!((li == ri) && (ls == rs) &&
              findStateChange(ll, rl).isEmpty)) {
              if (li <= i) atoms += LinkDeletion(agentOffset(la), j)
              fixLink(atoms, la, i, j, rl, ra2, ri, rj2)
              addLhsRhsMod(la, ra)
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
              addRhsMod(i)
            }
          }
          case _ => { }
        }
      }

      // Construct action
      new Action(lhs, rhs, atoms.toList, additions,
        lhsAgentsModified, rhsAgentsModified)
    }
  }
}
