package kappa

trait Actions {
  this: LanguageContext with Patterns with Mixtures with Embeddings
      with Rules =>

  // Actions
  final class Action(
    val lhs: Pattern,
    val rhs: Pattern,
    val linkDeletions: Seq[Action.LinkDeletion],
    val agentDeletions: Seq[Action.AgentDeletion],
    val agentAdditions: Seq[Action.AgentAddition],
    val linkAdditions: Seq[Action.LinkAddition],
    val agentStateChanges: Seq[Action.AgentStateChange],
    val siteStateChanges: Seq[Action.SiteStateChange],
    val linkStateChanges: Seq[Action.LinkStateChange])
      extends Function2[Embedding, Mixture, Unit] {

    def apply(embedding: Embedding, mixture: Mixture = mix) {

      @inline def agent(c: ComponentIndex, a: AgentIndex) = embedding(c)(a)

      for (ld <- linkDeletions) {
        mix disconnect (agent(ld.c, ld.a), ld.s)
      }
      for (ad <- agentDeletions) {
        mix -= (agent(ad.c, ad.a))
      }
      for (aa <- agentAdditions) {
        mix += (aa.state, aa.siteStates)
      }
      for (la <- linkAdditions) {
        mix connect (
          agent(la.c1, la.a1), la.s1, la.l1,
          agent(la.c2, la.a2), la.s2, la.l2)
      }

      // FIXME: Should maybe have a method in Mixture for those.
      for (as <- agentStateChanges) {
        agent(as.c, as.a).state = as.state
      }
      for (ss <- siteStateChanges) {
        agent(ss.c, ss.a).sites(ss.s).state = ss.state
      }
      for (ls <- linkStateChanges) {
        val s = agent(ls.c, ls.a)(ls.s)
        s.link = s.link match {
          case Mixture.Linked(a, s, _) => Mixture.Linked(a, s, ls.state)
          case _ => throw new IllegalArgumentException(
            "attempt to change state of stub")
        }
      }

      // FIXME: Handle positive/negative update.
    }

    def :@ (rate: => Double) = new Rule(this, ns => ns.product * rate)
    def !@ (law: (Int*) => Double) = new Rule(this, law) // see https:/ / github.com/jkrivine/KaSim/issues/9
  }

  object Action {

    sealed abstract class Atomic
    final case class LinkDeletion(
      c: ComponentIndex, a: AgentIndex, s: SiteIndex, hasSideEffect: Boolean)
        extends Atomic
    final case class AgentDeletion(c: ComponentIndex, a: AgentIndex)
        extends Atomic
    final case class AgentAddition(
      state: AgentState, siteStates: Seq[SiteState]) extends Atomic
    final case class LinkAddition(
      c1: ComponentIndex, a1: AgentIndex, s1: SiteIndex, l1: LinkState,
      c2: ComponentIndex, a2: AgentIndex, s2: SiteIndex, l2: LinkState) extends Atomic
    final case class AgentStateChange(
      c: ComponentIndex, a: AgentIndex, state: AgentState) extends Atomic
    final case class SiteStateChange(
      c: ComponentIndex, a: AgentIndex, s: SiteIndex, state: SiteState) extends Atomic
    final case class LinkStateChange(
      c: ComponentIndex, a: AgentIndex, s: SiteIndex, state: LinkState) extends Atomic

    def apply(
      lhs: Pattern,
      rhs: Pattern,
      linkDeletions: Seq[LinkDeletion],
      agentDeletions: Seq[AgentDeletion],
      agentAdditions: Seq[AgentAddition],
      linkAdditions: Seq[LinkAddition],
      agentStateChanges: Seq[AgentStateChange],
      siteStateChanges: Seq[SiteStateChange],
      linkStateChanges: Seq[LinkStateChange]) = new Action(
      lhs, rhs, linkDeletions, agentDeletions, agentAdditions, linkAdditions,
      agentStateChanges, siteStateChanges, linkStateChanges)

    // FIXME: Dummy, implement!
    def apply(lhs: Pattern, rhs: Pattern) = new Action(
      lhs, rhs, List(), List(), List(), List(), List(), List(), List())
  }
}
