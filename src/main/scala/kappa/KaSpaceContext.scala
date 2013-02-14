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
  type AgentState = KaSpaceAgentState
  type SiteState  = KaSpaceSiteState
  type LinkState  = KaSpaceLinkState

  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]) =
    KaSpaceAgentState(agentType, state)
  def mkSiteState(agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]) =
    KaSpaceSiteState(agentType, siteName, state)
  def mkLinkState(link: Link, state: Option[LinkStateName]) =
    KaSpaceLinkState(link, state)

  final case class KaSpaceAgentState(atype: AgentType, state: Option[AgentStateName])
      extends AgentStateIntf[KaSpaceAgentState]
  {
    val atypeSym: AgentTypeSym = agentTypeSyms(atype)

    // -- AgentStateIntf[KaSpaceAgentState] API --

    @inline def matchesInLongestCommonPrefix(that: KaSpaceAgentState) =
      this.atypeSym == that.atypeSym

    // -- Matchable[KaSpaceAgentState] API --

    @inline def matches(that: KaSpaceAgentState) =
      (this.atypeSym == that.atypeSym) &&
      Matchable.optionMatches(this.state, that.state)(_==_)

    @inline override def isEquivTo[U <: KaSpaceAgentState](that: U): Boolean =
      (this.atypeSym == that.atypeSym) && (this.state == that.state)

    @inline def join(that: KaSpaceAgentState) =
      if (this.atypeSym == that.atypeSym) (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceAgentState(atype, None))
      } else None

    @inline def meet(that: KaSpaceAgentState) =
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

  final case class KaSpaceSiteState(atype: AgentType,
                                 name: SiteName,
                                 state: Option[SiteStateName])
      extends Matchable[KaSpaceSiteState]
  {
    val nameSym: SiteNameSym = siteNameSyms(atype)(name)

    // -- Matchable[KaSpaceSiteState] API --

    @inline def matches(that: KaSpaceSiteState) =
      (this.nameSym == that.nameSym) &&
      Matchable.optionMatches(this.state, that.state)(_==_)

    @inline override def isEquivTo[U <: KaSpaceSiteState](that: U): Boolean =
      (this.nameSym == that.nameSym) && (this.state == that.state)

    @inline def join(that: KaSpaceSiteState) =
      if (this.nameSym == that.nameSym) (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceSiteState(atype, name, None))
      } else None

    @inline def meet(that: KaSpaceSiteState) =
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

  case class KaSpaceLinkState(link: Link, state: Option[LinkStateName])
    extends Matchable[KaSpaceLinkState]
  {
    // -- Matchable[KaSpaceLinkState] API --
    @inline def matches(that: KaSpaceLinkState) = this.state == that.state

    @inline override def isEquivTo[U <: KaSpaceLinkState](that: U): Boolean =
      this.state == that.state

    @inline def join(that: KaSpaceLinkState) =
      (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceLinkState(link, None))
      }

    @inline def meet(that: KaSpaceLinkState) =
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

