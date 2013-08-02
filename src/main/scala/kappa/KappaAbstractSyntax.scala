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
 */
trait KappaAbstractSyntax extends KappaLikeAbstractSyntax {
  this: KappaContext
      with ContactGraphs
      with Patterns
      with Mixtures
      with Actions
      with Rules
      with KappaParsers =>


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract Kappa agent states. */
  class AbstractKappaAgentState(val agentType: AgentTypeName)
      extends PartialAbstractKappaLikeAgentState {

    /** Creates an agent state from this abstract agent state. */
    @inline final def toAgentState: AgentState =
      KappaAgentState(findAgentStateSet)

    override def toString = agentType
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

    override def toString =
      name + (label map (stateDelim + _) getOrElse "")
  }

  /** Companion object of the AbstractKappaSiteState class. */
  object AbstractKappaSiteState {
    @inline def apply(
      name: SiteName, label: Option[SiteLabel]): AbstractKappaSiteState =
      new AbstractKappaSiteState(name, label)
  }

  /** A class representing abstract Kappa link states. */
  class AbstractKappaLinkState extends AbstractLinkState {

    /** Creates a link state from this abstract link state. */
    @inline final def toLinkState(linkId: Option[LinkId],
      source: SiteStateSet, target: SiteStateSet): LinkState =
      KappaLinkState(linkId)

    override def toString = ""
  }

  /** Companion object of the AbstractKappaLinkState class. */
  object AbstractKappaLinkState extends AbstractKappaLinkState


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

  /** A class representing abstract KappaLike links. */
  class AbstractKappaLinked(val id: LinkId) extends AbstractLinked {
    @inline final def state: AbstractLinkState = AbstractKappaLinkState

    override def toString = linkDelim + id
  }

  /** Companion object of the PartialAbstractKappaLinkState class. */
  object AbstractKappaLinked {
    @inline def apply(id: LinkId): AbstractKappaLinked =
      new AbstractKappaLinked(id)
  }


  // -- Sugar for pattern and mixture construction. --

  /** Convert site names into abstract site states. */
  implicit def siteNameToAbstractSiteState(
    n: SiteName): PartialAbstractKappaSiteState =
    PartialAbstractKappaSiteState(n)

  /** Convert link IDs into abstract links. */
  implicit def linkIdToAbstractLinkState(id: LinkId): AbstractKappaLinked =
    AbstractKappaLinked(id)


  /** Chain together a sequence of monomers. */
  def chain(monomers: Seq[AbstractPattern], prev: EndpointName,
    next: EndpointName): AbstractPattern = {
    val l = AbstractKappaLinkState
    monomers.tail.foldLeft(monomers.head) { (chain, ap) =>
      chain -< (next, l, l, prev) >- ap
    }
  }


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
}

