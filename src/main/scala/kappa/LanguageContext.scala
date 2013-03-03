package kappa

import scala.language.postfixOps

/** Generic language context trait. */
trait LanguageContext
{
  this: Parser with ContactGraph =>

  // -- State set types --
  type AgentStateSet <: GenericAgentStateSet
  type SiteStateSet <: GenericSiteStateSet
  type LinkStateSet <: GenericLinkStateSet


  /** Creates an agent state set from a set of agent state names. */
  def mkAgentStateSet(stateSet: AgentStateSetName): AgentStateSet

  /** Creates a site state set from a set of site state names. */
  def mkSiteStateSet(agentStateSet: AgentStateSet, siteStateSet: SiteStateSetName): SiteStateSet

  /** Creates a link state set from a set of link state names. */
  def mkLinkStateSet(source: SiteStateSet, target: SiteStateSet, stateSet: LinkStateSetName): LinkStateSet


  /** A trait for agent state sets. */
  trait GenericAgentStateSet { // extends Set[AgentState] with SetLike[AgentState]

    /** Tests whether this set contains a given agent state. */
    def contains(astate: AgentState): Boolean

    /** Tests whether this set is empty. (Better description required) */
    def isEmpty: Boolean

    /**
     * An iterable for the finite set of sites associated with
     * this agent state set.
     */
    //def siteStateSet: Iterable[SiteStateSet]

    /**
     * This method receives a partially defined, unordered interface
     * and returns a fully defined, ordered one. In other words, it
     * fills the given interface with the missing undefined sites.
     *
     * NOTE: The code that relies on the assumption that sites need
     * to be ordered is in Patterns.Pattern.Agent.matches.
     */
    def fillInterface(siteStates: Iterable[SiteState]): Seq[SiteState] = {
      val siteStateSets =
        for (siteStateSet <- contactGraph.siteStateSets
             if siteStateSet.agentStateSet == this)
        yield siteStateSet

      // FIXME Once a site state set is assigned a site,
      // it shouldn't be available anymore for assignment
      val siteMap: Map[SiteStateSet, SiteState] = {
        for { siteState <- siteStates
              siteStateSet <- siteStateSets
              if (siteStateSet contains siteState) }
        yield (siteStateSet -> siteState)
      }.toMap withDefault (_.undefinedSite)

      //siteStateSets.map(siteMap.lift).flatten
      siteStateSets map siteMap
    }
  }

  trait GenericSiteStateSet { // extends Set[SiteState] with SetLike[SiteState]

    /** Tests whether this set contains a given site state. */
    def contains(sstate: SiteState): Boolean

    /** Tests whether this set is empty. (Better description required) */
    def isEmpty: Boolean

    /** Returns an undefined site state. */
    def undefinedSite: SiteState

    /** Returns the agent state set to which this site state set is associated. */
    def agentStateSet: AgentStateSet
  }

  trait GenericLinkStateSet { // extends Set[LinkState] with SetLike[LinkState]

    /** Tests whether this set contains a given site state. */
    def contains(lstate: LinkState): Boolean

    /** Tests whether this set is empty. (Better description required) */
    def isEmpty: Boolean
  }


  // -- State types --
  type AgentState <: GenericAgentState[AgentState]
  type SiteState <: GenericSiteState[SiteState]
  type LinkState <: GenericLinkState[LinkState]


  /** Creates an agent state from an agent state name. */
  def mkAgentState(state: AgentStateName): AgentState

  /** Creates an site state from an site state name. */
  def mkSiteState(agentStateSet: AgentStateSet,
                  siteState: SiteStateName): SiteState

  // RHZ: Meanwhile I'm storing the source and target of a link
  // in the state itself, but that information should be stored
  // in the contact graph (ie as we do with Patterns).

  /** Creates an link state from an link state name. */
  def mkLinkState(source: SiteStateSet,
                  target: Option[SiteStateSet],
                  state: LinkStateName): LinkState


  /** A trait for generic agent states. */
  trait GenericAgentState[T] extends Matchable[T]
  {
    this: T =>

    /** Returns the agent state set this agent state belongs to. */
    def agentStateSet: AgentStateSet
  }

  /** A trait for generic site states. */
  trait GenericSiteState[T] extends Matchable[T]
  {
    this: T =>

    /** Returns the site state set this agent state belongs to. */
    def siteStateSet: SiteStateSet
  }

  /** A trait for generic link states. */
  trait GenericLinkState[T] extends Matchable[T]
  {
    this: T =>

    /** Returns the link state set this agent state belongs to. */
    def linkStateSet: LinkStateSet
  }
}


/** Language context for Kappa-like languages. */
trait KappaLikeContext extends LanguageContext
{
  this: KappaLikeParser with ContactGraph =>

  // -- State set types --
  type AgentStateSet <: KappaLikeAgentStateSet
  type SiteStateSet <: KappaLikeSiteStateSet
  type LinkStateSet <: KappaLikeLinkStateSet


  def optionContains[T](x: Option[T], xs: List[T]): Boolean =
    x map (xs contains _) getOrElse true


  /** Kappa-like agent state sets. */
  trait KappaLikeAgentStateSet extends GenericAgentStateSet
  {
    /** Returns the agent type associated with this agent state set. */
    def agentType: AgentType

    /** Returns the set of labels associated with this agent state set. */
    def labels: List[AgentLabel]

    // FIXME This doesn't work. Why?
    // Error: AgentState does not match KappaLikeAgentState[AgentState]
    // Is it because of covariant/contravariant issues?
    // If so, how can we work around it?
    /*
    def contains(astate: KappaLikeAgentState[AgentState]): Boolean =
      (agentType == astate.agentType) &&
      optionContains(astate.agentLabel, agentLabels)
    */

    @inline def isEmpty: Boolean = labels.isEmpty
  }

  /** A trait for Kappa-like site states sets. */
  trait KappaLikeSiteStateSet extends GenericSiteStateSet
  {
    /** Returns the site name associated with this site state set. */
    def siteName: SiteName

    /** Returns the set of labels associated with this site state set. */
    def labels: List[SiteLabel]

    // FIXME This doesn't work. See above
    /*
    def contains(sstate: KappaLikeSiteState[SiteState]): Boolean =
      (siteName == sstate.siteName) &&
      optionContains(sstate.siteLabel, siteLabels)
    */

    def isEmpty: Boolean = labels.isEmpty
  }

  trait KappaLikeLinkStateSet extends GenericLinkStateSet
  {
    /** Returns the set of labels associated with this link state set. */
    def labels: List[LinkLabel]

    // FIXME This doesn't work. See above
    /*
    def contains(lstate: KappaLikeLinkState[LinkState]): Boolean =
      optionContains(lstate.linkLabel, linkLabels)
    */

    def isEmpty: Boolean = labels.isEmpty
  }


  // -- State types --

  // Refined type bound for agent state type
  type AgentState <: KappaLikeAgentState[AgentState]
  type SiteState <: KappaLikeSiteState[SiteState]
  type LinkState <: KappaLikeLinkState[LinkState]

  /** A trait for Kappa-like agent states. */
  trait KappaLikeAgentState[T] extends GenericAgentState[T] {
    this: T =>

    /** Returns the agent type. */
    def agentType: AgentType

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

