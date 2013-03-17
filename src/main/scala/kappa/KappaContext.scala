package kappa

import scala.reflect.ClassTag


trait KappaContext extends KappaLikeContext {
  this: ContactGraphs with  KappaAbstractSyntax with KappaParsers =>

  // -- Constituents of site graph state types --
  type AgentLabel = Unit
  type  SiteLabel = String
  type  LinkLabel = Unit

  // -- State types --
  type AgentState = KappaAgentState
  type SiteState  = KappaSiteState
  type LinkState  = KappaLinkState

  /** An implicit providing a class tag for [[SiteState]]s. */
  implicit val siteStateClassTag = ClassTag[SiteState](classOf[SiteState])


  /** Kappa agent state. */
  final case class KappaAgentState(agentStateSet: KappaAgentStateSet)
      extends KappaLikeAgentState[KappaAgentState] {

    // -- KappaLikeAgentState[KappaAgentState] API --

    @inline def agentType = agentStateSet.agentType

    @inline def label = None

    @inline def matchesInLongestCommonPrefix(that: KappaAgentState) =
      this.agentStateSet == that.agentStateSet


    // -- Matchable[KappaAgentState] API --

    @inline def matches(that: KappaAgentState) =
      this.agentStateSet == that.agentStateSet

    @inline override def isEquivTo[U <: KappaAgentState](that: U): Boolean =
      this.agentStateSet == that.agentStateSet

    @inline def join(that: KappaAgentState) =
      if (this.agentStateSet == that.agentStateSet) Some(this) else None

    @inline def meet(that: KappaAgentState) =
      if (this.agentStateSet == that.agentStateSet) Some(this) else None

    @inline def isComplete = true


    // -- Any API --

    @inline override def toString = agentType
  }


  /** Kappa site state. */
  final case class KappaSiteState(
    siteStateSet: KappaSiteStateSet, internalState: Option[SiteLabel])
      extends KappaLikeSiteState[KappaSiteState] {

    val labelSym = internalState map siteStateSet.getLabelSym


    // -- KappaLikeAgentState[KappaAgentState] API --

    @inline def siteName = siteStateSet.siteName

    @inline def label = internalState

    // -- Matchable[KappaSiteState] API --

    @inline def matches(that: KappaSiteState) =
      (this.siteStateSet == that.siteStateSet) &&
      Matchable.optionMatches(this.labelSym, that.labelSym)(_==_)

    @inline override def isEquivTo[U <: KappaSiteState](that: U): Boolean =
      (this.siteStateSet == that.siteStateSet) && (this.labelSym == that.labelSym)

    @inline def join(that: KappaSiteState) =
      if (this.siteStateSet == that.siteStateSet) (this.labelSym, that.labelSym) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KappaSiteState(siteStateSet, None))
      } else None

    @inline def meet(that: KappaSiteState) =
      if (this.siteStateSet == that.siteStateSet) (this.labelSym, that.labelSym) match {
        case (None, _) => Some(that)
        case (_, None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    @inline def isComplete = siteStateSet.isEmpty || !internalState.isEmpty

    // -- Any API --

    @inline override def toString = siteName + (internalState map (":" + _) getOrElse "")
  }


  /** Kappa link state. */
  final case class KappaLinkState(linkName: Option[LinkId])
      extends KappaLikeLinkState[KappaLinkState] {
    // -- GenericLinkState[KappaLinkState] API --

    @inline def linkStateSet: KappaLinkStateSet = KappaLinkStateSet

    // -- KappaLikeLinkState[KappaLinkState] API --

    @inline def label: Option[LinkLabel] = None

    // -- Matchable[KappaLinkState] API --

    @inline def matches(that: KappaLinkState) = true

    @inline override def isEquivTo[U <: KappaLinkState](that: U): Boolean = true

    @inline def join(that: KappaLinkState) = Some(this)

    @inline def meet(that: KappaLinkState) = Some(this)

    @inline def isComplete = true

    // -- Any API --

    @inline override def toString = linkName map (_.toString) getOrElse ""
  }


  // -- State set types --
  type AgentStateSet = KappaAgentStateSet
  type SiteStateSet = KappaSiteStateSet
  type LinkStateSet = KappaLinkStateSet


  /** Creates an agent state set from a set of agent state names. */
  def mkAgentStateSet(agentStateSet: AgentStateSetName): AgentStateSet =
    KappaAgentStateSet(agentStateSet.agentType)

  /** Creates a site state set from a set of site state names. */
  def mkSiteStateSet(agentStateSet: AgentStateSet, siteStateSet: SiteStateSetName): SiteStateSet =
    KappaSiteStateSet(siteStateSet.siteName, siteStateSet.labels, agentStateSet)

  /** Creates a link state set from a set of link state names. */
  def mkLinkStateSet(source: SiteStateSet, target: SiteStateSet, stateSet: LinkStateSetName): LinkStateSet =
    KappaLinkStateSet


  /** Kappa agent state sets. */
  final case class KappaAgentStateSet(agentType: AgentTypeName)
      extends KappaLikeAgentStateSet {

    // -- KappaLikeLinkStateSet API --

    @inline def labels: List[AgentLabel] = List()

    // -- GenericLinkStateSet API --

    /** Tests whether this set contains a given site state. */
    @inline override def contains(astate: AgentState): Boolean =
      agentType == astate.agentType

    /** Tests whether this set is empty. (Better description required) */
    @inline override def isEmpty: Boolean = true
  }


  /** Kappa site state sets. */
  final case class KappaSiteStateSet(
    siteName: SiteName,
    internalStates: List[SiteLabel],
    agentStateSet: AgentStateSet)
      extends KappaLikeSiteStateSet {

    // Site label symbols
    type SiteLabelSym = Int
    private val labelSyms: Map[SiteLabel, SiteLabelSym] = internalStates.zipWithIndex.toMap
    @inline def getLabelSym(label: SiteLabel): SiteLabelSym = labelSyms(label)

    // -- KappaLikeSiteStateSet API --

    @inline def labels = internalStates

    // TODO This should be KappaLikeSiteStateName(siteName, internalStates.headOption)
    // if what we want to construct is a mixture agent to have KaSim-like semantics.
    @inline def undefinedSite: KappaSiteState =
      new AbstractKappaSiteState(siteName, None).toSiteState(agentStateSet)
  }


  /** Kappa link state sets. */
  sealed case class KappaLinkStateSet() extends KappaLikeLinkStateSet {

    // -- KappaLikeLinkStateSet API --

    @inline final def labels: List[LinkLabel] = List()

    // -- GenericLinkStateSet API --

    /** Tests whether this set contains a given site state. */
    @inline final override def contains(lstate: LinkState): Boolean = true

    /** Tests whether this set is empty. (Better description required) */
    @inline final override def isEmpty: Boolean = true
  }

  object KappaLinkStateSet extends KappaLinkStateSet
}

