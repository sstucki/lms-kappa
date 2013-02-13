package kappa

import breeze.linalg._

// RHZ: There is a lot of copy-paste and repetition here!
// There are things that we need to abstract out
trait KaSpaceContext extends LanguageContext {
  this: KaSpaceSymbols =>

  type AgentType = String
  type SiteName  = String

  // State types... The Name ending don't really
  // do justice here to what these types are
  type AgentStateName = Double
  type SiteStateName  = DenseVector[Double]
  type LinkStateName  = DenseMatrix[Double]

  // Composite state types
  type AgentState = AgentStateImpl
  type SiteState  = SiteStateImpl
  type LinkState  = LinkStateImpl

  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]) =
    AgentStateImpl(agentType, state)
  def mkSiteState(agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]) =
    SiteStateImpl(agentType, siteName, state)
  def mkLinkState(link: Link, state: Option[LinkStateName]) =
    LinkStateImpl(link, state)

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

