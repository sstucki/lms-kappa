package kappa

import scala.reflect.ClassTag


// RHZ: There is a lot of copy-paste and repetition here!
// There are things that we need to abstract out
trait KaSpaceContext extends KappaLikeContext {
  this: KaSpaceSymbols =>

  type AgentType = String
  type SiteName  = String

  // State types... The Name ending don't really
  // do justice here to what these types are
  type AgentStateName = Double
  type  SiteStateName = Position
  type  LinkStateName = Orientation

  // Composite state types
  type AgentState = KaSpaceAgentState
  type  SiteState = KaSpaceSiteState
  type  LinkState = KaSpaceLinkState

  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]) =
    KaSpaceAgentState(agentType, state)
  def mkSiteState(agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]) =
    KaSpaceSiteState(agentType, siteName, state)
  def mkLinkState(link: LinkId, state: Option[LinkStateName]) =
    KaSpaceLinkState(link, state)

  /** An implicit providing a class tag for [[SiteState]]s. */
  implicit def siteStateClassTag = implicitly[ClassTag[SiteState]]


  final case class KaSpaceAgentState(
    atype: AgentType, radius: Option[AgentStateName],
    orientation: Orientation = Orientation(),
    position: Position = Position(0, 0, 0))
      extends AgentStateIntf[KaSpaceAgentState] {

    val atypeSym: AgentTypeSym = agentTypeSyms(atype)

    // -- AgentStateIntf[KaSpaceAgentState] API --

    @inline def matchesInLongestCommonPrefix(that: KaSpaceAgentState) =
      this.atypeSym == that.atypeSym

    // -- Matchable[KaSpaceAgentState] API --

    @inline def matches(that: KaSpaceAgentState) =
      (this.atypeSym == that.atypeSym) &&
      Matchable.optionMatches(this.radius, that.radius)(_==_)

    @inline override def isEquivTo[U <: KaSpaceAgentState](that: U): Boolean =
      (this.atypeSym == that.atypeSym) && (this.radius == that.radius)

    @inline def join(that: KaSpaceAgentState) =
      if (this.atypeSym == that.atypeSym) (this.radius, that.radius) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceAgentState(atype, None))
      } else None

    @inline def meet(that: KaSpaceAgentState) =
      if (this.atypeSym == that.atypeSym) (this.radius, that.radius) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    @inline def isComplete = !hasAgentStateNames(atype) || !radius.isEmpty

    // -- Any API --

    @inline override def toString = atype + (radius map (":" + _) getOrElse "")
  }

  final case class KaSpaceSiteState(atype: AgentType,
                                 name: SiteName,
                                 position: Option[SiteStateName])
      extends Matchable[KaSpaceSiteState]
  {
    val nameSym: SiteNameSym = siteNameSyms(atype)(name)

    // -- Matchable[KaSpaceSiteState] API --

    @inline def matches(that: KaSpaceSiteState) =
      (this.nameSym == that.nameSym) &&
      Matchable.optionMatches(this.position, that.position)(_==_)

    @inline override def isEquivTo[U <: KaSpaceSiteState](that: U): Boolean =
      (this.nameSym == that.nameSym) && (this.position == that.position)

    @inline def join(that: KaSpaceSiteState) =
      if (this.nameSym == that.nameSym) (this.position, that.position) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceSiteState(atype, name, None))
      } else None

    @inline def meet(that: KaSpaceSiteState) =
      if (this.nameSym == that.nameSym) (this.position, that.position) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    @inline def isComplete =
      !hasSiteStateNames(atype)(name) || !position.isEmpty

    // -- Any API --

    @inline override def toString =
      name + (position map (":" + _) getOrElse "")
  }

  case class KaSpaceLinkState(link: LinkId, orientation: Option[LinkStateName])
    extends Matchable[KaSpaceLinkState]
  {
    // -- Matchable[KaSpaceLinkState] API --
    @inline def matches(that: KaSpaceLinkState) =
      Matchable.optionMatches(this.orientation, that.orientation)(_==_)

    @inline override def isEquivTo[U <: KaSpaceLinkState](that: U): Boolean =
      this.orientation == that.orientation

    @inline def join(that: KaSpaceLinkState) =
      (this.orientation, that.orientation) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceLinkState(link, None))
      }

    @inline def meet(that: KaSpaceLinkState) =
      (this.orientation, that.orientation) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      }

    @inline def isComplete = !hasLinkStateNames(link) || !orientation.isEmpty

    // -- Any API --
    @inline override def toString = orientation map (_.toString) getOrElse ""
  }
}

