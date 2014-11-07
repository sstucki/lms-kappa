package kappa

import scala.reflect.ClassTag


trait KappaContext extends KappaLikeContext {
  this: ContactGraphs
      with  KappaAbstractSyntax =>

  type AgentLabel = Unit
  type SiteLabel = String
  type LinkLabel = Unit


  // -- State set types --

  type AgentStateSet = KappaAgentStateSet
  type SiteStateSet = KappaSiteStateSet
  type LinkStateSet = KappaLinkStateSet

  /** Kappa agent state sets. */
  final case class KappaAgentStateSet(agentName: AgentName)
      extends KappaLikeAgentStateSet {

    // -- KappaLikeLinkStateSet API --

    @inline def labels: List[AgentLabel] = List()
    @inline def undefinedState = KappaAgentState(this)
    @inline def defaultState = KappaAgentState(this)

    // -- GenericLinkStateSet API --

    /** Tests whether this set contains a given site state. */
    @inline override def contains(astate: AgentState): Boolean =
      agentName == astate.agentName

    /** Tests whether this set is empty. (Better description required) */
    @inline override def isEmpty: Boolean = true
  }


  /** Kappa site state sets. */
  final case class KappaSiteStateSet(siteName: SiteName,
    labels: List[SiteLabel])
      extends KappaLikeSiteStateSet {

    // -- SiteLabel symbol table --

    type SiteLabelSym = Int

    private val labelSyms: Map[SiteLabel, SiteLabelSym] =
      labels.zipWithIndex.toMap

    @inline def getLabelSym(label: SiteLabel): SiteLabelSym =
      labelSyms.getOrElse(label, throw new IllegalArgumentException(
        "internal state '" + label + "' not found for site '" + this))

    // -- KappaLikeSiteStateSet API --

    @inline def undefinedState = KappaSiteState(this, None)

    // NOTE: For mixture agents to have KaSim-like semantics.
    @inline def defaultState = KappaSiteState(this, labels.headOption)
  }


  /** Kappa link state sets. */
  sealed case class KappaLinkStateSet()
      extends KappaLikeLinkStateSet {

    // -- KappaLikeLinkStateSet API --

    @inline final def labels: List[LinkLabel] = List()
    @inline def undefinedState = KappaLinkState(None)
    @inline def defaultState = KappaLinkState(None)

    // -- GenericLinkStateSet API --

    /** Tests whether this set contains a given site state. */
    @inline final override def contains(that: LinkState): Boolean = true

    /** Tests whether this set is empty. */
    @inline final override def isEmpty: Boolean = true
  }

  object KappaLinkStateSet extends KappaLinkStateSet


  // -- State types --

  type AgentState = KappaAgentState
  type SiteState  = KappaSiteState
  type LinkState  = KappaLinkState

  /** An implicit providing a class tag for [[SiteState]]s. */
  implicit val siteStateClassTag = ClassTag[SiteState](classOf[SiteState])


  /** Kappa agent state. */
  final case class KappaAgentState(set: KappaAgentStateSet)
      extends KappaLikeAgentState[KappaAgentState] {

    // -- KappaLikeAgentState[KappaAgentState] API --

    @inline def agentName = set.agentName

    @inline def label = None

    @inline def matchesInLongestCommonPrefix(that: KappaAgentState) =
      this.set == that.set


    // -- Matchable[KappaAgentState] API --

    @inline def matches(that: KappaAgentState) = this.set == that.set

    @inline override def isEquivTo[U <: KappaAgentState](that: U)
        : Boolean = this.set == that.set

    @inline def join(that: KappaAgentState) =
      if (this.set == that.set) Some(this) else None

    @inline def meet(that: KappaAgentState) =
      if (this.set == that.set) Some(this) else None

    @inline def isComplete = true


    // -- Any API --

    @inline override def toString = agentName
  }


  /** Kappa site state. */
  final case class KappaSiteState(set: KappaSiteStateSet,
    label: Option[SiteLabel])
      extends KappaLikeSiteState[KappaSiteState] {

    val labelSym = label map set.getLabelSym

    // -- KappaLikeAgentState[KappaAgentState] API --

    @inline def siteName = set.siteName

    // -- Matchable[KappaSiteState] API --

    @inline def matches(that: KappaSiteState) =
      (this.set == that.set) &&
      Matchable.optionMatches(this.labelSym, that.labelSym)(_==_)

    @inline override def isEquivTo[U <: KappaSiteState](that: U)
        : Boolean =
      (this.set == that.set) && (this.labelSym == that.labelSym)

    @inline def join(that: KappaSiteState) =
      if (this.set == that.set) (this.labelSym, that.labelSym) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KappaSiteState(set, None))
      } else None

    @inline def meet(that: KappaSiteState) =
      if (this.set == that.set) (this.labelSym, that.labelSym) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this)
                                     else None
      } else None

    @inline def isComplete = set.isEmpty || !label.isEmpty

    // -- Any API --

    @inline override def toString =
      siteName + (label map (stateDelim + _) getOrElse "")
  }


  /** Kappa link state. */
  final case class KappaLinkState(linkName: Option[LinkName])
      extends KappaLikeLinkState[KappaLinkState] {

    // -- GenericLinkState[KappaLinkState] API --

    @inline def id: LinkId = linkName getOrElse (
      throw new IllegalStateException("no link id"))

    @inline def withLinkId(linkId: LinkId): KappaLinkState =
      KappaLinkState(Some(linkId))

    @inline def set: KappaLinkStateSet = KappaLinkStateSet

    // -- KappaLikeLinkState[KappaLinkState] API --

    @inline def label: Option[LinkLabel] = None

    // -- Matchable[KappaLinkState] API --

    @inline def matches(that: KappaLinkState) = true

    @inline override def isEquivTo[U <: KappaLinkState](that: U)
        : Boolean = true

    @inline def join(that: KappaLinkState) = Some(this)

    @inline def meet(that: KappaLinkState) = Some(this)

    @inline def isComplete = true

    // -- Any API --

    @inline override def toString =
      linkName map (_.toString) getOrElse ""
  }
}

