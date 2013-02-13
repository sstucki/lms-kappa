package kappa

trait KappaContext extends LanguageContext {
  this: KappaSymbols =>

  type AgentType = String
  type SiteName  = String

  // State types
  type AgentStateName = Unit
  type SiteStateName  = String
  type LinkStateName  = Unit

  // Composite state types
  type AgentState = AgentStateImpl
  type SiteState  = SiteStateImpl
  type LinkState  = LinkStateImpl

  // RHZ: We discard states in agents and links because Kappa doesn't have them
  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]) =
    AgentStateImpl(agentType)
  def mkSiteState(agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]) =
    SiteStateImpl(agentType, siteName, state)
  def mkLinkState(link: Link, state: Option[LinkStateName]) = LinkStateImpl()

  // TODO: If we were to use dedicated symbol classes rather than Ints
  // to represent symbols, some of these wrappers would likely not be
  // necessary as we could use the symbols directly to represent the
  // states (in cases where the states are not tuples).
  final case class AgentStateImpl(atype: AgentType)
      extends AgentStateIntf[AgentStateImpl]
  {
    val atypeSym: AgentTypeSym = agentTypeSyms(atype)

    // -- AgentStateIntf[AgentStateImpl] API --

    @inline def matchesInLongestCommonPrefix(that: AgentStateImpl) =
      this.atypeSym == that.atypeSym

    // -- Matchable[AgentStateImpl] API --

    @inline def matches(that: AgentStateImpl) = this.atypeSym == that.atypeSym

    @inline override def isEquivTo[U <: AgentStateImpl](that: U): Boolean =
      this.atypeSym == that.atypeSym

    @inline def join(that: AgentStateImpl) =
      if (this.atypeSym == that.atypeSym) Some(this) else None

    @inline def meet(that: AgentStateImpl) =
      if (this.atypeSym == that.atypeSym) Some(this) else None

    val isComplete = true

    // -- Any API --
    @inline override def toString = atype
  }

  final case class SiteStateImpl(atype: AgentType,
                                 name: SiteName,
                                 state: Option[SiteStateName])
      extends Matchable[SiteStateImpl]
  {
    val nameSym: SiteNameSym = siteNameSyms(atype)(name)
    val stateSym: Option[SiteStateNameSym] = state map siteStateNameSyms(atype)(name)

    // -- Matchable[SiteStateImpl] API --

    @inline def matches(that: SiteStateImpl) =
      (this.nameSym == that.nameSym) &&
      Matchable.optionMatches(this.stateSym, that.stateSym)(_==_)

    @inline override def isEquivTo[U <: SiteStateImpl](that: U): Boolean =
      (this.nameSym == that.nameSym) && (this.stateSym == that.stateSym)

    @inline def join(that: SiteStateImpl) =
      if (this.nameSym == that.nameSym) (this.stateSym, that.stateSym) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(SiteStateImpl(atype, name, None))
      } else None

    @inline def meet(that: SiteStateImpl) =
      if (this.nameSym == that.nameSym) (this.stateSym, that.stateSym) match {
        case (None, None) => Some(this)
        case (None, Some(s2)) => Some(that)
        case (Some(s2), None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    val noStateInCG = siteStateNameSyms(atype)(name).isEmpty
    val isComplete = noStateInCG || !state.isEmpty

    // -- Any API --

    override def toString = name + (state map (":" + _) getOrElse "")
  }

  case class LinkStateImpl()
    extends Matchable[LinkStateImpl]
  {
    // -- Matchable[LinkStateImpl] API --
    @inline def matches(that: LinkStateImpl) = true

    @inline override def isEquivTo[U <: LinkStateImpl](that: U): Boolean = true

    @inline def join(that: LinkStateImpl) = Some(LinkStateImpl())

    @inline def meet(that: LinkStateImpl) = Some(LinkStateImpl())

    val isComplete = true

    // -- Any API --
    override def toString = ""
  }
}

