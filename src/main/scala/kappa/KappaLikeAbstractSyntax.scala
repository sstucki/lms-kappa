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
      with Patterns
      with Mixtures
      with Actions
      with Rules
      with KappaLikeParsers =>


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing partial abstract KappaLike agent states. */
  abstract class PartialAbstractKappaLikeAgentState
      extends AbstractAgentState {

    /** The agent type of this KappaLike agent state. */
    def agentType: AgentTypeName

    /** Find an agent state set in the contact graph. */
    def findAgentStateSet: AgentStateSet =
      contactGraph.agentStateSets find (_.agentType == agentType) getOrElse {
        throw new IllegalArgumentException(
          "couldn't find agent state set for \"" + agentType + "\"")
      }
  }

  /** A class representing abstract KappaLike site states. */
  abstract class PartialAbstractKappaLikeSiteState extends AbstractSiteState {

    /** The site name of this KappaLike site state. */
    def name: SiteName

    /** Find a site state in the contact graph. */
    def findSiteStateSet(agentStateSet: AgentStateSet): SiteStateSet =
      contactGraph.siteStateSets find { siteStateSet =>
        (siteStateSet.siteName      == name) &&
        (siteStateSet.agentStateSet == agentStateSet)
    } getOrElse {
      throw new IllegalArgumentException(
        "couldn't find site state set for \"" + name + "\"")
    }
  }
}

