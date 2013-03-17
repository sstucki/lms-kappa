package kappa


/** Language context for Kappa-like languages. */
trait KappaLikeContext extends LanguageContext {
  this: ContactGraphs with KappaLikeAbstractSyntax with KappaLikeParsers =>

  // -- State set types --
  type AgentStateSet <: KappaLikeAgentStateSet
  type SiteStateSet <: KappaLikeSiteStateSet
  type LinkStateSet <: KappaLikeLinkStateSet


  def optionContains[T](x: Option[T], xs: List[T]): Boolean =
    x map (xs contains _) getOrElse true


  /** Kappa-like agent state sets. */
  trait KappaLikeAgentStateSet extends GenericAgentStateSet {

    /** Returns the agent type associated with this agent state set. */
    def agentType: AgentTypeName

    /** Returns the set of labels associated with this agent state set. */
    def labels: List[AgentLabel]

    @inline def contains(agentState: AgentState): Boolean =
      (agentType == agentState.agentType) &&
      optionContains(agentState.label, labels)

    @inline def isEmpty: Boolean = labels.isEmpty
  }

  /** A trait for Kappa-like site states sets. */
  trait KappaLikeSiteStateSet extends GenericSiteStateSet {

    /** Returns the site name associated with this site state set. */
    def siteName: SiteName

    /** Returns the set of labels associated with this site state set. */
    def labels: List[SiteLabel]

    @inline def contains(siteState: SiteState): Boolean =
      (siteName == siteState.siteName) &&
      optionContains(siteState.label, labels)

    @inline def isEmpty: Boolean = labels.isEmpty
  }

  trait KappaLikeLinkStateSet extends GenericLinkStateSet {

    /** Returns the set of labels associated with this link state set. */
    def labels: List[LinkLabel]

    @inline def contains(linkState: LinkState): Boolean =
      optionContains(linkState.label, labels)

    @inline def isEmpty: Boolean = labels.isEmpty
  }


  // -- Constituents of site graph state types --
  type AgentTypeName = String
  type SiteName = String
  type AgentLabel
  type SiteLabel
  type LinkLabel


  // -- State types --

  // Refined type bound for agent state type
  type AgentState <: KappaLikeAgentState[AgentState]
  type SiteState <: KappaLikeSiteState[SiteState]
  type LinkState <: KappaLikeLinkState[LinkState]

  /** A trait for Kappa-like agent states. */
  trait KappaLikeAgentState[T] extends GenericAgentState[T] {
    this: T =>

    /** Returns the agent type. */
    def agentType: AgentTypeName

    /** Returns the agent label. */
    def label: Option[AgentLabel]

    /**
     * Checks if this agent state matches `that` when constructing
     * rules according to the "longest common prefix" rule.
     *
     * @return `true` if this agent state matches `that`.
     */
    def matchesInLongestCommonPrefix(that: T): Boolean

    // TODO matches and other methods can be defined here
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

