package kappa

/** A trait for agent states. */
trait AgentStateIntf[T] extends Matchable[T] {
  this: T =>

  /**
   * Checks if this agent state matches `that` when constructing rules
   * according to the "longest common prefix" rule.
   *
   * @return `true` if this agent state matches `that`.
   */
  def matchesInLongestCommonPrefix(that: T): Boolean
}


trait LanguageContext {
  type AgentType
  type SiteName

  type Link = (AgentType, SiteName, AgentType, SiteName)

  // State types
  type AgentStateName
  type SiteStateName
  type LinkStateName

  def invertLinkStateName(state: LinkStateName): LinkStateName

  // Composite state types
  type AgentState <: AgentStateIntf[AgentState]
  type SiteState <: Matchable[SiteState]
  type LinkState <: Matchable[LinkState]

  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]): AgentState
  def mkSiteState(agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]): SiteState
  def mkLinkState(link: Link, state: Option[LinkStateName]): LinkState
}


trait KappaContext extends LanguageContext {
  this: KappaSymbols =>

  type AgentType = String
  type SiteName  = String

  // State types
  type AgentStateName = Unit
  type SiteStateName  = String
  type LinkStateName  = Unit

  def invertLinkStateName(state: LinkStateName) = state

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

// RHZ: There is a lot of copy-paste and repetition here!
// There are things that we need to abstract out
trait KaSpaceContext extends LanguageContext {
  this: KaSpaceSymbols =>

  type AgentType = String
  type SiteName  = String

  // State types... The Name ending don't really
  // do justice here to what these types are
  type AgentStateName = Double
  type SiteStateName  = Vector[Double]
  type LinkStateName  = Vector[Vector[Double]]

  // invertMatrix is probably implemented in Scalala so no need to reimplement
  def invertLinkStateName(state: LinkStateName) = state // invertMatrix(state)

  // Composite state types
  type AgentState = AgentStateImpl
  type SiteState  = SiteStateImpl
  type LinkState  = LinkStateImpl

  // RHZ: We discard states in agents and links because Kappa doesn't have them
  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]) =
    AgentStateImpl(agentType, state)
  def mkSiteState(agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]) =
    SiteStateImpl(agentType, siteName, state)
  def mkLinkState(link: Link, state: Option[LinkStateName]) = LinkStateImpl(link, state)

  // TODO: If we were to use dedicated symbol classes rather than Ints
  // to represent symbols, some of these wrappers would likely not be
  // necessary as we could use the symbols directly to represent the
  // states (in cases where the states are not tuples).
  final case class AgentStateImpl(atype: AgentType, state: Option[AgentStateName])
      extends AgentStateIntf[AgentStateImpl]
  {
    val atypeSym: AgentTypeSym = agentTypeSyms(atype)

    // -- AgentStateIntf[AgentStateImpl] API --

    @inline def matchesInLongestCommonPrefix(that: AgentStateImpl) =
      this.atypeSym == that.atypeSym

    // -- Matchable[AgentStateImpl] API --

    @inline def matches(that: AgentStateImpl) =
      (this.atypeSym == that.atypeSym) &&
      Matchable.optionMatches(this.state, that.state)(_==_)

    @inline override def isEquivTo[U <: AgentStateImpl](that: U): Boolean =
      (this.atypeSym == that.atypeSym) && (this.state == that.state)

    @inline def join(that: AgentStateImpl) =
      if (this.atypeSym == that.atypeSym) (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(AgentStateImpl(atype, None))
      } else None

    @inline def meet(that: AgentStateImpl) =
      if (this.atypeSym == that.atypeSym) (this.state, that.state) match {
        case (None, None) => Some(this)
        case (None, Some(s2)) => Some(that)
        case (Some(s2), None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    val isComplete = !hasAgentStateNames(atype) || !state.isEmpty

    // -- Any API --

    @inline override def toString = atype + (state map (":" + _) getOrElse "")
  }

  final case class SiteStateImpl(atype: AgentType,
                                 name: SiteName,
                                 state: Option[SiteStateName])
      extends Matchable[SiteStateImpl]
  {
    val nameSym: SiteNameSym = siteNameSyms(atype)(name)

    // -- Matchable[SiteStateImpl] API --

    @inline def matches(that: SiteStateImpl) =
      (this.nameSym == that.nameSym) &&
      Matchable.optionMatches(this.state, that.state)(_==_)

    @inline override def isEquivTo[U <: SiteStateImpl](that: U): Boolean =
      (this.nameSym == that.nameSym) && (this.state == that.state)

    @inline def join(that: SiteStateImpl) =
      if (this.nameSym == that.nameSym) (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(SiteStateImpl(atype, name, None))
      } else None

    @inline def meet(that: SiteStateImpl) =
      if (this.nameSym == that.nameSym) (this.state, that.state) match {
        case (None, None) => Some(this)
        case (None, Some(s2)) => Some(that)
        case (Some(s2), None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    val isComplete = !hasSiteStateNames(atype)(name) || !state.isEmpty

    // -- Any API --

    override def toString = name + (state map (":" + _) getOrElse "")
  }

  case class LinkStateImpl(link: Link, state: Option[LinkStateName])
    extends Matchable[LinkStateImpl]
  {
    // -- Matchable[LinkStateImpl] API --
    @inline def matches(that: LinkStateImpl) = this.state == that.state

    @inline override def isEquivTo[U <: LinkStateImpl](that: U): Boolean =
      this.state == that.state

    @inline def join(that: LinkStateImpl) =
      (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(LinkStateImpl(link, None))
      }

    @inline def meet(that: LinkStateImpl) =
      (this.state, that.state) match {
        case (None, None) => Some(this)
        case (None, Some(s2)) => Some(that)
        case (Some(s2), None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      }

    val isComplete = !hasLinkStateNames(link) || !state.isEmpty

    // -- Any API --
    override def toString = state map (_.toString) getOrElse ""
  }
}

