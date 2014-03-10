package kappa


/** Language context for Kappa-like languages. */
trait KappaLikeContext extends LanguageContext {
  this: ContactGraphs
      with KappaLikeAbstractSyntax =>

  type AgentName = String
  type SiteName = String
  type LinkName = LinkId

  type AgentLabel
  type SiteLabel
  type LinkLabel


  @inline final def optionContains[T](x: Option[T], xs: List[T])
      : Boolean = x map (xs contains _) getOrElse true


  // -- State set types --

  type AgentStateSet <: KappaLikeAgentStateSet
  type SiteStateSet <: KappaLikeSiteStateSet
  type LinkStateSet <: KappaLikeLinkStateSet

  /** Kappa-like agent state sets. */
  trait KappaLikeAgentStateSet extends GenericAgentStateSet {

    /** Returns the agent type associated with this agent state set. */
    def agentName: AgentName

    /** Returns the set of labels associated with this agent state set. */
    def labels: List[AgentLabel]

    @inline def contains(that: AgentState): Boolean =
      (agentName == that.agentName) &&
      optionContains(that.label, labels)

    @inline def isEmpty: Boolean = labels.isEmpty
  }

  /** A trait for Kappa-like site states sets. */
  trait KappaLikeSiteStateSet extends GenericSiteStateSet {

    /** Returns the site name associated with this site state set. */
    def siteName: SiteName

    /** Returns the set of labels associated with this site state set. */
    def labels: List[SiteLabel]

    @inline def contains(that: SiteState): Boolean =
      (siteName == that.siteName) &&
      optionContains(that.label, labels)

    @inline def isEmpty: Boolean = labels.isEmpty
  }

  trait KappaLikeLinkStateSet extends GenericLinkStateSet {

    /** Returns the set of labels associated with this link state set. */
    def labels: List[LinkLabel]

    @inline def contains(that: LinkState): Boolean =
      optionContains(that.label, labels)

    @inline def isEmpty: Boolean = labels.isEmpty
  }


  // -- State types --

  type AgentState <: KappaLikeAgentState[AgentState]
  type SiteState <: KappaLikeSiteState[SiteState]
  type LinkState <: KappaLikeLinkState[LinkState]

  /** A trait for Kappa-like agent states. */
  trait KappaLikeAgentState[T] extends GenericAgentState[T] {
    this: T =>

    /** Returns the agent type. */
    def agentName: AgentName

    /** Returns the agent label. */
    def label: Option[AgentLabel]

    /**
     * Checks if this agent state matches `that` when constructing
     * rules according to the "longest common prefix" rule.
     *
     * @return `true` if this agent state matches `that`.
     */
    def matchesInLongestCommonPrefix(that: T): Boolean

    // TODO: matches and other methods can be defined here
  }

  /** A trait for Kappa-like site states. */
  trait KappaLikeSiteState[T] extends GenericSiteState[T] {
    this: T =>

    /** Returns the site name. */
    def siteName: SiteName

    /** Returns the site label. */
    def label: Option[SiteLabel]
  }

  /** A trait for Kappa-like site states. */
  trait KappaLikeLinkState[T] extends GenericLinkState[T] {
    this: T =>

    /** Returns the link label. */
    def label: Option[LinkLabel]
  }
}

