package kappa

import scala.reflect.ClassTag


/** Generic language context trait. */
trait LanguageContext {

  val stateDelim = "~"
  val linkDelim = "!"
  val endpointDelim = "/"
  val undefined = "?"


  // -- State set types --

  type AgentStateSet <: GenericAgentStateSet
  type SiteStateSet <: GenericSiteStateSet
  type LinkStateSet <: GenericLinkStateSet


  // /** Creates an agent state set from a set of agent state names. */
  // def mkAgentStateSet(stateSet: AgentStateSetName): AgentStateSet

  // /** Creates a site state set from a set of site state names. */
  // def mkSiteStateSet(agentStateSet: AgentStateSet, siteStateSet: SiteStateSetName): SiteStateSet

  // /** Creates a link state set from a set of link state names. */
  // def mkLinkStateSet(source: SiteStateSet, target: SiteStateSet, stateSet: LinkStateSetName): LinkStateSet


  /** A trait for agent state sets. */
  trait GenericAgentStateSet {
    // extends Set[AgentState] with SetLike[AgentState]

    /** Tests whether this set contains a given agent state. */
    def contains(state: AgentState): Boolean

    /** Tests whether this set is empty. (Better description required) */
    def isEmpty: Boolean

    /** Returns an undefined agent state. */
    def undefinedState: AgentState

    /** Returns the default agent state for this agent state set. */
    def defaultState: AgentState
  }

  trait GenericSiteStateSet {
    // extends Set[SiteState] with SetLike[SiteState]

    /** Tests whether this set contains a given site state. */
    def contains(sstate: SiteState): Boolean

    /** Tests whether this set is empty. (Better description required) */
    def isEmpty: Boolean

    /** Returns an undefined site state. */
    def undefinedState: SiteState

    /** Returns the default site state for this site state set. */
    def defaultState: SiteState

    // /** Returns the agent state set to which this site state set is associated. */
    // def agentStateSet: AgentStateSet
  }

  trait GenericLinkStateSet {
    // extends Set[LinkState] with SetLike[LinkState]

    /** Tests whether this set contains a given site state. */
    def contains(lstate: LinkState): Boolean

    /** Tests whether this set is empty. (Better description required) */
    def isEmpty: Boolean

    /** Returns an undefined link state. */
    def undefinedState: LinkState

    /** Returns the default link state for this link state set. */
    def defaultState: LinkState
  }


  // -- State types --
  type AgentState <: GenericAgentState[AgentState]
  type SiteState <: GenericSiteState[SiteState]
  type LinkState <: GenericLinkState[LinkState]


  /** A trait for generic agent states. */
  trait GenericAgentState[T] extends Matchable[T] {
    this: T =>

    // /** Returns the agent state set this agent state belongs to. */
    // def agentStateSet: AgentStateSet
  }

  /** A trait for generic site states. */
  trait GenericSiteState[T] extends Matchable[T] {
    this: T =>

    // /** Returns the site state set this agent state belongs to. */
    // def siteStateSet: SiteStateSet
  }

  /** A trait for generic link states. */
  trait GenericLinkState[T] extends Matchable[T] {
    this: T =>

    /** Returns the link id. */
    def id: LinkId

    /** Returns a link state with a different link id. */
    def withLinkId(linkId: LinkId): T

    // /** Returns the link state set this agent state belongs to. */
    // def linkStateSet: LinkStateSet
  }

  /** An implicit providing a class tag for [[SiteState]]s. */
  implicit def siteStateClassTag: ClassTag[SiteState]
}
