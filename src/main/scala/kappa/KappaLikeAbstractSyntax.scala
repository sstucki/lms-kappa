package kappa

import scala.collection.mutable


/**
 * Abstract syntax tree structures for Kappa-like languages.
 *
 * NOTE: The classes in this trait can be used as "pattern
 * combinators", but they are very different form the generic pattern
 * builder in the [[Patterns]] trait.  In fact they are more like the
 * nodes of a pattern AST, enhanced with combinator-like operators.
 */
trait KappaLikeAbstractSyntax extends AbstractSyntax {
  this: KappaLikeContext
      with ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures
      with Actions
      with Rules =>

  import SiteGraph.{AgentType, SiteType}

  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract KappaLike agent states. */
  abstract class AbstractKappaLikeAgentState
      extends AbstractAgentState {

    /** The agent name of this KappaLike agent state. */
    def agentName: AgentName

    /** The agent label of this KappaLike agent state. */
    def label: Option[AgentLabel]

    val agentType: AgentType =
      contactGraph.agents find (_.states.agentName == agentName) getOrElse {
        throw new IllegalArgumentException(
          "couldn't find agent type for \"" + agentName + "\"")
      }
  }

  /** A class representing abstract KappaLike site states. */
  abstract class AbstractKappaLikeSiteState
      extends AbstractSiteState {

    /** The site name of this KappaLike site state. */
    def siteName: SiteName

    def siteType(agentType: ContactGraph.Agent): SiteType =
      agentType.sites find (_.states.siteName == siteName) getOrElse {
        throw new IllegalArgumentException(
          "couldn't find site type for \"" + siteName +
            "\" on agent " + agentType)
      }

    /** The site label of this KappaLike site state. */
    def label: Option[SiteLabel]
  }
}

