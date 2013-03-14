package kappa

import scala.collection.mutable


/**
 * Abstract syntax tree structures for Kappa-like languages.
 *
 * NOTE: The classes in this trait can be used as "pattern
 * combinators", but they are very different form the generic pattern
 * builder in the [[Patterns]] trait.  In fact they are more like the
 * nodes of a pattern AST, enhanced with combinator-like operators.
 *
 * FIXME: These classes should probably be merged with the
 * [[KaSpaceParser.AST]] classes (or vice-versa) for consistency.
 */
trait KaSpaceAbstractSyntax extends KappaLikeAbstractSyntax {
  this: KaSpaceContext
      with ContactGraphs
      with Patterns
      with KaSpaceActions
      with Mixtures
      with KaSpaceParser =>


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract KaSpace agent states. */
  final case class AbstractKaSpaceAgentState(
    atype: AgentType, radius: Option[AgentLabel])
      extends AbstractKappaLikeAgentState {

    /** Creates an agent state from this abstract agent state. */
    def toAgentState: AgentState =
      KaSpaceAgentState(findAgentStateSet, radius)
  }

  /** A class representing abstract KaSpace site states. */
  final case class AbstractKaSpaceSiteState(
    name: SiteName, position: Option[SiteLabel])
      extends AbstractKappaLikeSiteState {

    /** Creates a site state from this abstract site state. */
    def toSiteState(agentStateSet: AgentStateSet): SiteState =
      KaSpaceSiteState(findSiteStateSet(agentStateSet), position)
  }

  /** A class representing abstract KaSpace link states. */
  final case class AbstractKaSpaceLinkState(
    name: Option[LinkName], orientation: Option[LinkLabel])
      extends AbstractLinkState {

    /** Creates a link state from this abstract link state. */
    def toLinkState(source: SiteStateSet, target: SiteStateSet): LinkState = {

      // Find a link state set in the contact graph.
      val lss = contactGraph.linkStateSets.find { linkStateSet =>
        (linkStateSet.source == source) &&
        (linkStateSet.target == target) &&
        (orientation map { w1 =>
          linkStateSet.labels exists { w2 => w1 ~= w2 }
        } getOrElse true)
      } getOrElse {
        throw new IllegalArgumentException(
          "couldn't find link state set from " + source +
            " to " + target + " with orientation " + orientation)
      }
      KaSpaceLinkState(lss, orientation)
    }
  }


  // -- Sugar for pattern and mixture construction. --

  // FIXME: These should become redundant once the Parser.AST has been
  // replaced by Abstract* classes.
  def agentStateNameToAbstractAgentState(n: AgentStateName): AbstractAgentState =
    AbstractKaSpaceAgentState(n.agentType, n.label)
  def siteStateNameToAbstractSiteState(n: SiteStateName): AbstractSiteState =
    AbstractKaSpaceSiteState(n.siteName, n.label)
  def linkStateNameToAbstractLinkState(n: LinkStateName): AbstractLinkState =
    AbstractKaSpaceLinkState(n.linkName, n.label)
}

