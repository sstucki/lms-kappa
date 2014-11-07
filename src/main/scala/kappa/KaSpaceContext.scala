package kappa

import scala.reflect.ClassTag


trait KaSpaceContext extends KappaLikeContext {
  this: ContactGraphs
      with KaSpaceAbstractSyntax =>

  type AgentLabel = Double
  type SiteLabel = Position
  type LinkLabel = Orientation

  // -- State set types --

  type AgentStateSet = KaSpaceAgentStateSet
  type SiteStateSet = KaSpaceSiteStateSet
  type LinkStateSet = KaSpaceLinkStateSet

  /** KaSpace agent state set. */
  final case class KaSpaceAgentStateSet(
    agentName: AgentName,
    radii: List[AgentLabel])
      extends KappaLikeAgentStateSet {

    // -- KappaLikeAgentStateSet API --

    @inline def labels: List[AgentLabel] = radii
    @inline def undefinedState = KaSpaceAgentState(this, None)
    @inline def defaultState = KaSpaceAgentState(this, labels.headOption)
  }


  /** KaSpace site state set. */
  final case class KaSpaceSiteStateSet(
    siteName: SiteName,
    positions: List[SiteLabel])
      extends KappaLikeSiteStateSet {

    // -- SiteLabel symbol table --

    type SiteLabelSym = Int

    private val labelSyms: Map[SiteLabel, SiteLabelSym] =
      positions.zipWithIndex.toMap

    @inline def getLabelSym(label: SiteLabel): SiteLabelSym =
      labelSyms.getOrElse(label, throw new IllegalArgumentException(
        "position '" + label + "' not found for site '" + this))

    // -- KappaLikeSiteStateSet API --

    @inline def labels = positions
    @inline def undefinedState = KaSpaceSiteState(this, None)
    @inline def defaultState = KaSpaceSiteState(this, labels.headOption)
  }


  /** KaSpace link state set. */
  final case class KaSpaceLinkStateSet(orientations: List[LinkLabel])
      extends KappaLikeLinkStateSet {

    // -- LinkLabel symbol table --

    type LinkLabelSym = Int

    private val labelSyms: Map[LinkLabel, LinkLabelSym] =
      orientations.zipWithIndex.toMap

    @inline def getLabelSym(label: LinkLabel): LinkLabelSym =
      labelSyms.getOrElse(label, throw new IllegalArgumentException(
        "orientation '" + label + "' not found for site '" + this))

    // -- KappaLikeLinkStateSet API --

    @inline def labels = orientations
    @inline def undefinedState = KaSpaceLinkState(this, None)
    @inline def defaultState = KaSpaceLinkState(this, labels.headOption)

    // -- Any API --

    @inline override def toString = orientations.toString
  }


  // -- State types --

  type AgentState = KaSpaceAgentState
  type SiteState = KaSpaceSiteState
  type LinkState = KaSpaceLinkState


  /** An implicit providing a class tag for [[SiteState]]s. */
  implicit val siteStateClassTag = ClassTag[SiteState](classOf[SiteState])


  /** KaSpace agent state. */
  final case class KaSpaceAgentState(
    set: KaSpaceAgentStateSet,
    radius: Option[AgentLabel],
    orientation: Orientation = Orientation(),
    position: Position = Position(0, 0, 0))
      extends KappaLikeAgentState[KaSpaceAgentState] {

    // TODO: Should I use label syms here? Is comparing Ints much
    // faster than Doubles?

    // -- KappaLikeAgentState[KaSpaceAgentState] API --

    @inline def agentName = set.agentName

    @inline def label = radius

    @inline def matchesInLongestCommonPrefix(that: KaSpaceAgentState) =
      this.set == that.set


    // -- Matchable[KaSpaceAgentState] API --

    @inline def matches(that: KaSpaceAgentState) =
      (this.set == that.set) &&
      Matchable.optionMatches(this.radius, that.radius)(_==_)

    @inline override def isEquivTo[U <: KaSpaceAgentState](that: U)
        : Boolean =
      (this.set == that.set) && (this.radius == that.radius)

    @inline def join(that: KaSpaceAgentState) =
      if (this.set == that.set) (this.radius, that.radius) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceAgentState(set, None))
      } else None

    @inline def meet(that: KaSpaceAgentState) =
      if (this.set == that.set) (this.radius, that.radius) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this)
                                     else None
      } else None

    @inline def isComplete = set.isEmpty || !radius.isEmpty

    // -- Any API --

    @inline override def toString =
      agentName + (radius map (stateDelim + _) getOrElse "")
  }


  /** KaSpace site state. */
  final case class KaSpaceSiteState(
    set: KaSpaceSiteStateSet,
    position: Option[SiteLabel])
      extends KappaLikeSiteState[KaSpaceSiteState] {

    val positionSym = position map set.getLabelSym

    // -- KappaLikeSiteState[KaSpaceSiteState] API --

    @inline def siteName = set.siteName

    @inline def label = position

    // -- Matchable[KaSpaceSiteState] API --

    @inline def matches(that: KaSpaceSiteState) =
      (this.set == that.set) &&
      Matchable.optionMatches(this.positionSym, that.positionSym)(_==_)

    @inline override def isEquivTo[U <: KaSpaceSiteState](that: U)
        : Boolean =
      (this.set == that.set) && (this.positionSym == that.positionSym)

    @inline def join(that: KaSpaceSiteState) =
      if (this.set == that.set)
        (this.positionSym, that.positionSym) match {
          case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
          case _ => Some(KaSpaceSiteState(set, None))
        } else None

    @inline def meet(that: KaSpaceSiteState) =
      if (this.set == that.set)
        (this.positionSym, that.positionSym) match {
          case (None, _) => Some(that)
          case (_, None) => Some(this)
          case (Some(s1), Some(s2)) => if (s1 == s2) Some(this)
                                       else None
        } else None

    @inline def isComplete = set.isEmpty || !position.isEmpty

    // -- Any API --

    @inline override def toString =
      siteName + (position map (stateDelim + _) getOrElse "")
  }

  /** KaSpace link state. */
  final case class KaSpaceLinkState(
    set: LinkStateSet,
    orientation: Option[LinkLabel],
    linkId: Option[LinkId] = None)
      extends KappaLikeLinkState[KaSpaceLinkState] {

    val orientationSym = orientation map set.getLabelSym

    // -- KappaLikeLinkState[KaSpaceLinkState] API --

    @inline def id: LinkId = linkId getOrElse (
      throw new IllegalStateException("no link id"))

    @inline def withLinkId(linkId: LinkId): KaSpaceLinkState =
      KaSpaceLinkState(set, orientation, Some(linkId))

    @inline def label = orientation

    // -- Matchable[KaSpaceLinkState] API --
    @inline def matches(that: KaSpaceLinkState) =
      Matchable.optionMatches(this.orientationSym,
        that.orientationSym)(_==_)

    @inline override def isEquivTo[U <: KaSpaceLinkState](that: U)
        : Boolean =
      this.orientationSym == that.orientationSym

    @inline def join(that: KaSpaceLinkState) =
      (this.orientationSym, that.orientationSym) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KaSpaceLinkState(set, None, None))
      }

    @inline def meet(that: KaSpaceLinkState) =
      (this.orientationSym, that.orientationSym) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this)
                                     else None
      }

    @inline def isComplete = set.isEmpty || !orientation.isEmpty

    // -- Any API --
    @inline override def toString = (linkId, orientation) match {
      case (Some(id), Some(w)) => id + stateDelim + w
      case (Some(id), None) => id.toString
      case (None, Some(w)) => w.toString
      case (None, None) => ""
    }
  }
}

