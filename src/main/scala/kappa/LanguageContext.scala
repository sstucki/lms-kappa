package kappa

import scala.reflect.ClassTag


/** Generic language context trait. */
trait LanguageContext {

  type AgentType
  type SiteName

  type LinkId = (AgentType, SiteName, AgentType, SiteName)

  // State types
  type AgentStateName
  type SiteStateName
  type LinkStateName

  // Composite state types
  type AgentState <: Matchable[AgentState]
  type SiteState  <: Matchable[SiteState]
  type LinkState  <: Matchable[LinkState]

  def mkAgentState(agentType: AgentType, state: Option[AgentStateName]): AgentState
  def mkSiteState (agentType: AgentType, siteName: SiteName, state: Option[SiteStateName]): SiteState
  def mkLinkState(link: LinkId, state: Option[LinkStateName]): LinkState

  /** An implicit providing a class tag for [[SiteState]]s. */
  implicit def siteStateClassTag: ClassTag[SiteState]
}

/** Language context for Kappa-like languages. */
trait KappaLikeContext extends LanguageContext
{
  /** A trait for agent states. */
  trait AgentStateIntf[T] extends Matchable[T] {
    this: T =>

    /**
     * Checks if this agent state matches `that` when constructing
     * rules according to the "longest common prefix" rule.
     *
     * @return `true` if this agent state matches `that`.
     */
    def matchesInLongestCommonPrefix(that: T): Boolean
  }

  // Refined type bound for agent state type
  type AgentState <: AgentStateIntf[AgentState]
}

