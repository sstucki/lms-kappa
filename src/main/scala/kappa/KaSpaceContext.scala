package kappa

trait KaSpaceContext extends KappaLikeContext
{
  this: KaSpaceParser with ContactGraph =>

  // -- State types --
  type AgentState = KaSpaceAgentState
  type  SiteState = KaSpaceSiteState
  type  LinkState = KaSpaceLinkState


  /** Creates an agent state from an agent state name. */
  def mkAgentState(state: KappaLikeAgentStateName) =
    KaSpaceAgentState(findAgentStateSet(state), state.label)

  /** Creates an site state from an site state name. */
  def mkSiteState(agentStateSet: AgentStateSet, siteState: KappaLikeSiteStateName) =
    KaSpaceSiteState(findSiteStateSet(agentStateSet, siteState), siteState.label)

  /** Creates an link state from an link state name. */
  def mkLinkState(source: SiteStateSet, target: Option[SiteStateSet], state: LinkStateName) =
    KaSpaceLinkState(findLinkStateSet(source, target, state), state.label)


  /** KaSpace agent state. */
  final case class KaSpaceAgentState(
    val agentStateSet: KaSpaceAgentStateSet,
    val radius: Option[AgentLabel],
    val orientation: Orientation = Orientation(),
    val position: Position = Position(0, 0, 0))
      extends KappaLikeAgentState[KaSpaceAgentState]
  {
    // -- KappaLikeAgentState[KaSpaceAgentState] API --

    @inline def agentType = agentStateSet.agentType

    @inline def label = radius

    @inline def matchesInLongestCommonPrefix(that: KaSpaceAgentState) =
      this.agentStateSet == that.agentStateSet

    // -- Matchable[KaSpaceAgentState] API --

    @inline def matches(that: KaSpaceAgentState) =
      (this.agentStateSet == that.agentStateSet) &&
      Matchable.optionMatches(this.radius, that.radius)(_==_)

    @inline override def isEquivTo[U <: KaSpaceAgentState](that: U): Boolean =
      (this.agentStateSet == that.agentStateSet) && (this.radius == that.radius)

    @inline def join(that: KaSpaceAgentState) =
      if (this.agentStateSet == that.agentStateSet) (this.radius, that.radius) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceAgentState(agentStateSet, None))
      } else None

    @inline def meet(that: KaSpaceAgentState) =
      if (this.agentStateSet == that.agentStateSet) (this.radius, that.radius) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    @inline def isComplete = agentStateSet.isEmpty || !radius.isEmpty

    // -- Any API --

    @inline override def toString =
      agentStateSet.agentType + (radius map (":" + _) getOrElse "")
  }


  /** KaSpace site state. */
  final case class KaSpaceSiteState(
    val siteStateSet: KaSpaceSiteStateSet,
    val position: Option[SiteLabel])
      extends KappaLikeSiteState[KaSpaceSiteState]
  {
    // TODO Use label symbols

    // -- KappaLikeSiteState[KaSpaceSiteState] API --

    @inline def siteName = siteStateSet.siteName

    @inline def label = position

    // -- Matchable[KaSpaceSiteState] API --

    @inline def matches(that: KaSpaceSiteState) =
      (this.siteStateSet == that.siteStateSet) &&
      Matchable.optionMatches(this.position, that.position)(_==_)

    @inline override def isEquivTo[U <: KaSpaceSiteState](that: U): Boolean =
      (this.siteStateSet == that.siteStateSet) && (this.position == that.position)

    @inline def join(that: KaSpaceSiteState) =
      if (this.siteStateSet == that.siteStateSet) (this.position, that.position) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceSiteState(siteStateSet, None))
      } else None

    @inline def meet(that: KaSpaceSiteState) =
      if (this.siteStateSet == that.siteStateSet) (this.position, that.position) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    @inline def isComplete = siteStateSet.isEmpty || !position.isEmpty

    // -- Any API --

    @inline override def toString =
      siteStateSet.siteName + (position map (":" + _) getOrElse "")
  }


  /** KaSpace link state. */
  case class KaSpaceLinkState(
    val linkStateSet: LinkStateSet,
    val orientation: Option[LinkLabel])
      extends KappaLikeLinkState[KaSpaceLinkState]
  {
    // TODO Use label symbols

    // -- KappaLikeLinkState[KaSpaceLinkState] API --

    @inline def label = orientation

    // -- Matchable[KaSpaceLinkState] API --
    @inline def matches(that: KaSpaceLinkState) =
      Matchable.optionMatches(this.orientation, that.orientation)(_==_)

    @inline override def isEquivTo[U <: KaSpaceLinkState](that: U): Boolean =
      this.orientation == that.orientation

    @inline def join(that: KaSpaceLinkState) =
      (this.orientation, that.orientation) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceLinkState(linkStateSet, None))
      }

    @inline def meet(that: KaSpaceLinkState) =
      (this.orientation, that.orientation) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      }

    @inline def isComplete = linkStateSet.isEmpty || !orientation.isEmpty

    // -- Any API --
    @inline override def toString = orientation map (_.toString) getOrElse ""
  }


  // -- State set types --
  type AgentStateSet = KaSpaceAgentStateSet
  type SiteStateSet = KaSpaceSiteStateSet
  type LinkStateSet = KaSpaceLinkStateSet


  /** Creates an agent state set from a set of agent state names. */
  def mkAgentStateSet(agentStateSet: AgentStateSetName): AgentStateSet =
    KaSpaceAgentStateSet(agentStateSet.agentType, agentStateSet.labels)

  /** Creates a site state set from a set of site state names. */
  def mkSiteStateSet(agentStateSet: AgentStateSet,
                     siteStateSet: SiteStateSetName): SiteStateSet =
    KaSpaceSiteStateSet(siteStateSet.siteName, siteStateSet.labels, agentStateSet)

  /** Creates a link state set from a set of link state names. */
  def mkLinkStateSet(source: SiteStateSet,
                     target: SiteStateSet,
                     stateSet: LinkStateSetName): LinkStateSet =
    KaSpaceLinkStateSet(source, target, stateSet.labels)


  /** KaSpace agent state set. */
  final case class KaSpaceAgentStateSet(val agentType: AgentType,
                                        val radii: List[AgentLabel])
      extends KappaLikeAgentStateSet
  {
    // -- KappaLikeLinkStateSet API --

    @inline def labels: List[AgentLabel] = radii

    @inline def contains(agentState: KaSpaceAgentState): Boolean =
      (agentType == agentState.agentType) &&
      optionContains(agentState.label, labels)
  }


  /** KaSpace site state set. */
  final case class KaSpaceSiteStateSet(val siteName: SiteName,
                                       val positions: List[SiteLabel],
                                       val agentStateSet: AgentStateSet)
      extends KappaLikeSiteStateSet
  {
    // Site label symbols
    type SiteLabelSym = Int
    private val labelSyms: Map[SiteLabel, SiteLabelSym] = positions.zipWithIndex.toMap
    @inline def getLabelSym(label: SiteLabel): SiteLabelSym = labelSyms(label)

    // -- KappaLikeSiteStateSet API --

    @inline def labels = positions

    @inline def contains(siteState: KaSpaceSiteState): Boolean =
      (siteName == siteState.siteName) &&
      optionContains(siteState.label, labels)

    // TODO This should be KappaLikeSiteStateName(siteName, positions.headOption)
    // if what we want to construct is a mixture agent to have KaSim-like semantics.
    @inline def undefinedSite: SiteState = mkSiteState(
      agentStateSet, KappaLikeSiteStateName(siteName, None))
  }


  /** KaSpace link state set. */
  final case class KaSpaceLinkStateSet(val source: SiteStateSet,
                                       val target: SiteStateSet,
                                       val orientations: List[LinkLabel])
      extends KappaLikeLinkStateSet
  {
    // Link label symbols
    type LinkLabelSym = Int
    private val labelSyms: Map[LinkLabel, LinkLabelSym] = orientations.zipWithIndex.toMap
    @inline def getLabelSym(label: LinkLabel): LinkLabelSym = labelSyms(label)

    // -- KappaLikeLinkStateSet API --

    @inline def labels = orientations

    @inline def contains(linkState: KaSpaceLinkState): Boolean =
      optionContains(linkState.label, labels)

    // -- Any API --

    @inline override def toString = orientations.toString
  }
}

