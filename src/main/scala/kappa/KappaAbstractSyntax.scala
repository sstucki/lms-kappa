package kappa

import scala.collection.mutable

import scala.language.implicitConversions


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
      with Rules
      with KappaParser =>


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract Kappa agent states. */
  class AbstractKappaAgentState(val agentType: AgentTypeName)
      extends PartialAbstractKappaLikeAgentState {

    /** Creates an agent state from this abstract agent state. */
    @inline final def toAgentState: AgentState =
      KappaAgentState(findAgentStateSet)
  }

  /** Companion object of the AbstractKappaAgentState class. */
  object AbstractKappaAgentState {
    @inline def apply(agentType: AgentTypeName): AbstractKappaAgentState =
      new AbstractKappaAgentState(agentType)
  }

  /** A class representing abstract Kappa site states. */
  class AbstractKappaSiteState(
    val name: SiteName, val label: Option[SiteLabel])
      extends PartialAbstractKappaLikeSiteState {

    /** Creates a site state from this abstract site state. */
    @inline final def toSiteState(agentStateSet: AgentStateSet): SiteState =
      KappaSiteState(findSiteStateSet(agentStateSet), label)
  }

  /** Companion object of the AbstractKappaSiteState class. */
  object AbstractKappaSiteState {
    @inline def apply(
      name: SiteName, label: Option[SiteLabel]): AbstractKappaSiteState =
      new AbstractKappaSiteState(name, label)
  }

  /** A class representing abstract Kappa link states. */
  class AbstractKappaLinkState(val id: LinkId)
      extends AbstractLinkState {

    /** Creates a link state from this abstract link state. */
    @inline final def toLinkState(
      source: SiteStateSet, target: SiteStateSet): LinkState =
      KappaLinkState(Some(id))
  }

  /** Companion object of the AbstractKappaLinkState class. */
  object AbstractKappaLinkState {
    @inline def apply(id: LinkId): AbstractKappaLinkState =
      new AbstractKappaLinkState(id)
  }


  /** A class to build abstract Kappa site states. */
  class PartialAbstractKappaSiteState(val name: SiteName)
      extends PartialAbstractKappaLikeSiteState {

    /** Build an abstract site state with a particular label. */
    @inline final def ~(label: SiteLabel): AbstractSiteState =
      new AbstractKappaSiteState(name, Some(label))

    @inline final def toSiteState(agentStateSet: AgentStateSet): SiteState =
      new AbstractKappaSiteState(name, None).toSiteState(agentStateSet)
  }

  /** Companion object of the PartialAbstractKappaSiteState class. */
  object PartialAbstractKappaSiteState {
    @inline def apply(name: SiteName): PartialAbstractKappaSiteState =
      new PartialAbstractKappaSiteState(name)
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


  /** Convert site names into abstract site states. */
  implicit def siteNameToAbstractSiteState(
    n: SiteName): PartialAbstractKappaSiteState =
    PartialAbstractKappaSiteState(n)

  /** Convert link IDs into abstract link states. */
  implicit def linkIdToAbstractLinkState(id: LinkId): AbstractKappaLinkState =
    AbstractKappaLinkState(id)


  /**
   * Alias for easy agent state creation.
   *
   * Use case:
   * {{{
   *   // Define an object `A` representing an agent of type "A".
   *   object A extends AgentType("A")
   *
   *   // Define a val `x` representing a site with name "x".
   *   val x = Site("x")
   *
   *   // Use `A` and `x` to constructor an agent.
   *   withObs{ A(x) }
   * }}}
   */
  class AgentType(agentTypeName: AgentTypeName)
      extends AbstractKappaAgentState(agentTypeName)

  /**
   * A factory object for easy site state creation.
   *
   * Use case:
   * {{{
   *   // Define an object `A` representing an agent of type "A".
   *   object A extends AgentType("A")
   *
   *   // Define a val `x` representing a site with name "x".
   *   val x = Site("x")
   *
   *   // Use `A` and `x` to constructor an agent.
   *   withObs{ A(x) }
   * }}}
   */
  object Site {
    def apply(n: SiteName) = PartialAbstractKappaSiteState(n)
  }


  // FIXME: These should become redundant once the Parser.AST has been
  // replaced by Abstract* classes.
  def agentStateNameToAbstractAgentState(n: AgentStateName): AbstractAgentState =
    AbstractKappaAgentState(n.agentType)
  def siteStateNameToAbstractSiteState(n: SiteStateName): AbstractSiteState =
    AbstractKappaSiteState(n.siteName, n.label)
  def linkStateNameToAbstractLinkState(n: LinkStateName): AbstractLinkState =
    AbstractKappaLinkState(n.id)
}

