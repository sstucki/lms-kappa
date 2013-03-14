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
 * [[KappaParser.AST]] classes (or vice-versa) for consistency.
 */
trait KappaAbstractSyntax extends KappaLikeAbstractSyntax {
  this: KappaContext
      with ContactGraphs
      with Patterns
      with Mixtures
      with KappaActions
      with KappaParser =>


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract Kappa agent states. */
  final case class AbstractKappaAgentState(atype: AgentType)
      extends AbstractKappaLikeAgentState {

    /** Creates an agent state from this abstract agent state. */
    def toAgentState: AgentState =
      KappaAgentState(findAgentStateSet)
  }

  /** A class representing abstract Kappa site states. */
  final case class AbstractKappaSiteState(
    name: SiteName, internalState: Option[SiteLabel])
      extends AbstractKappaLikeSiteState {

    @inline def !(name: LinkName): AbstractSite =
      AbstractSiteBuilder(this).!(name, AbstractKappaLinkState(Some(name)))

    /** Creates a site state from this abstract site state. */
    def toSiteState(agentStateSet: AgentStateSet): SiteState =
      KappaSiteState(findSiteStateSet(agentStateSet), internalState)
  }

  /** A class representing abstract Kappa link states. */
  final case class AbstractKappaLinkState(name: Option[LinkName])
      extends AbstractLinkState {

    /** Creates a link state from this abstract link state. */
    def toLinkState(source: SiteStateSet, target: SiteStateSet): LinkState =
      KappaLinkState(name)
  }


  // -- Sugar for pattern and mixture construction. --

  // rhz: I want to be able to leave "holes" in a SiteGraphBuilder where
  // later I connect sites
  // Perhaps the only thing I need is to be able to "glue" two patterns
  // with one semi-link each. Or maybe I need a "hole" type where I can
  // put stubs, wildcards, links, anything, maybe even states.
  // Example of what I want:
  //   pp"A(c!_, x!1), _(a!1)" connect "C(a!_)" apply "B"
  // Or: pp"A(x!_)" free
  // Or: pp"A(x!_)" wildcard (Some("A", None), None, None)


  // FIXME: These should become redundant once the Parser.AST has been
  // replaced by Abstract* classes.
  def agentStateNameToAbstractAgentState(n: AgentStateName): AbstractAgentState =
    AbstractKappaAgentState(n.agentType)
  def siteStateNameToAbstractSiteState(n: SiteStateName): AbstractSiteState =
    AbstractKappaSiteState(n.siteName, n.label)
  def linkStateNameToAbstractLinkState(n: LinkStateName): AbstractLinkState =
    AbstractKappaLinkState(n.linkName)
}

