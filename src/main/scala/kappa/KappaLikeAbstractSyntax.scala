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


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract KappaLike agent states. */
  abstract class AbstractKappaLikeAgentState
      extends AbstractAgentState {

    /** The agent name of this KappaLike agent state. */
    def agentName: AgentName

    def label: Option[AgentLabel]

    val agentType: ContactGraph.Agent = {
      contactGraph.agents find (_.states.agentName == agentName)
    } getOrElse {
      throw new IllegalArgumentException(
        "couldn't find agent type for \"" + agentName + "\"")
    }

    // /** Find an agent state set in the contact graph. */
    // def findAgentStateSet: AgentStateSet =
    //   contactGraph.agentStateSets find (_.agentType == agentType) getOrElse {
    //     throw new IllegalArgumentException(
    //       "couldn't find agent state set for \"" + agentType + "\"")
    //   }

    /** A class representing abstract KappaLike site states. */
    abstract class AbstractKappaLikeSiteState
        extends AbstractSiteState {

      /** The site name of this KappaLike site state. */
      def siteName: SiteName

      def label: Option[SiteLabel]

      val siteType: agentType.Site = {
        agentType.sites find (_.states.siteName == siteName)
      } getOrElse {
        throw new IllegalArgumentException(
          "couldn't find site type for \"" + siteName +
            "\" on agent " + agentType)
      }

      // /** Find a site state in the contact graph. */
      // def findSiteStateSet(agentStateSet: AgentStateSet): SiteStateSet =
      //   contactGraph.siteStateSets find { siteStateSet =>
      //     (siteStateSet.siteName      == name) &&
      //     (siteStateSet.agentStateSet == agentStateSet)
      // } getOrElse {
      //   throw new IllegalArgumentException(
      //     "couldn't find site state set for \"" + name + "\"")
      // }
    }
  }
}

