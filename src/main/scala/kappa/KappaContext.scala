package kappa

import scala.reflect.ClassTag


trait KappaContext extends KappaLikeContext
{
  this: KappaSymbols =>

  type AgentType = String
  type SiteName  = String

  // State types
  type AgentStateName = Unit
  type SiteStateName  = String
  type LinkStateName  = Unit

  // Composite state types
  type AgentState = KappaAgentState
  type SiteState  = KappaSiteState
  type LinkState  = KappaLinkState

  // RHZ: We discard states in agents and links because Kappa doesn't have them
  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]) =
    KappaAgentState(agentType)
  def mkSiteState(agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]) =
    KappaSiteState(agentType, siteName, state)
  def mkLinkState(link: LinkId, state: Option[LinkStateName]) =
    KappaLinkState

  /** An implicit providing a class tag for [[SiteState]]s. */
  implicit val siteStateClassTag = ClassTag[SiteState](classOf[SiteState])


  // TODO: If we were to use dedicated symbol classes rather than Ints
  // to represent symbols, some of these wrappers would likely not be
  // necessary as we could use the symbols directly to represent the
  // states (in cases where the states are not tuples).
  final case class KappaAgentState(atype: AgentType)
      extends AgentStateIntf[KappaAgentState]
  {
    val atypeSym: AgentTypeSym = agentTypeSyms(atype)

    // -- AgentStateIntf[KappaAgentState] API --

    @inline def matchesInLongestCommonPrefix(that: KappaAgentState) =
      this.atypeSym == that.atypeSym

    // -- Matchable[KappaAgentState] API --

    @inline def matches(that: KappaAgentState) = this.atypeSym == that.atypeSym

    @inline override def isEquivTo[U <: KappaAgentState](that: U): Boolean =
      this.atypeSym == that.atypeSym

    @inline def join(that: KappaAgentState) =
      if (this.atypeSym == that.atypeSym) Some(this) else None

    @inline def meet(that: KappaAgentState) =
      if (this.atypeSym == that.atypeSym) Some(this) else None

    @inline def isComplete = true

    // -- Any API --

    @inline override def toString = atype
  }

  final case class KappaSiteState(
    atype: AgentType, name: SiteName, state: Option[SiteStateName])
      extends Matchable[KappaSiteState] {
    val nameSym: SiteNameSym = siteNameSyms(atype)(name)
    val stateSym: Option[SiteStateNameSym] = state map siteStateNameSyms(atype)(name)

    // -- Matchable[KappaSiteState] API --

    @inline def matches(that: KappaSiteState) =
      (this.nameSym == that.nameSym) &&
      Matchable.optionMatches(this.stateSym, that.stateSym)(_==_)

    @inline override def isEquivTo[U <: KappaSiteState](that: U): Boolean =
      (this.nameSym == that.nameSym) && (this.stateSym == that.stateSym)

    @inline def join(that: KappaSiteState) =
      if (this.nameSym == that.nameSym) (this.stateSym, that.stateSym) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KappaSiteState(atype, name, None))
      } else None

    @inline def meet(that: KappaSiteState) =
      if (this.nameSym == that.nameSym) (this.stateSym, that.stateSym) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    val noStateInCG = siteStateNameSyms(atype)(name).isEmpty
    @inline def isComplete = noStateInCG || !state.isEmpty

    // -- Any API --

    @inline override def toString = name + (state map (":" + _) getOrElse "")
  }

  sealed case class KappaLinkState() extends Matchable[KappaLinkState] {
    // -- Matchable[KappaLinkState] API --
    @inline final def matches(that: KappaLinkState) = true

    @inline final override def isEquivTo[U <: KappaLinkState](that: U): Boolean = true

    @inline final def join(that: KappaLinkState) = Some(KappaLinkState)

    @inline final def meet(that: KappaLinkState) = Some(KappaLinkState)

    @inline final def isComplete = true

    // -- Any API --

    @inline final override def toString = ""
  }

  object KappaLinkState extends KappaLinkState
}

