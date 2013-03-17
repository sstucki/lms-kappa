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
trait KaSpaceAbstractSyntax extends KappaLikeAbstractSyntax {
  this: KaSpaceContext
      with ContactGraphs
      with Patterns
      with KaSpaceActions
      with Mixtures
      with Rules
      with KaSpaceParsers =>


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract KaSpace agent states. */
  class AbstractKaSpaceAgentState(
    val agentType: AgentTypeName, val radius: Option[AgentLabel])
      extends PartialAbstractKappaLikeAgentState {

    /** Creates an agent state from this abstract agent state. */
    @inline final def toAgentState: AgentState =
      KaSpaceAgentState(findAgentStateSet, radius)
  }

  /** Companion object of the AbstractKaSpaceAgentState class. */
  object AbstractKaSpaceAgentState {
    @inline def apply(agentType: AgentTypeName, radius: Option[AgentLabel])
        : AbstractKaSpaceAgentState =
      new AbstractKaSpaceAgentState(agentType, radius)
  }

  /** A class representing abstract KaSpace site states. */
  class AbstractKaSpaceSiteState(
    val name: SiteName, val position: Option[SiteLabel])
      extends PartialAbstractKappaLikeSiteState {

    /** Creates a site state from this abstract site state. */
    @inline final def toSiteState(agentStateSet: AgentStateSet): SiteState =
      KaSpaceSiteState(findSiteStateSet(agentStateSet), position)
  }

  /** Companion object of the AbstractKaSpaceSiteState class. */
  object AbstractKaSpaceSiteState {
    @inline def apply(
      name: SiteName, position: Option[SiteLabel]): AbstractKaSpaceSiteState =
      new AbstractKaSpaceSiteState(name, position)
  }

  /** A class representing abstract KaSpace link states. */
  class AbstractKaSpaceLinkState(val orientation: Option[LinkLabel])
      extends AbstractLinkState {

    /** Creates a link state from this abstract link state. */
    @inline final def toLinkState(
      source: SiteStateSet, target: SiteStateSet): LinkState = {

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

  /** Companion object of the AbstractKaSpaceLinkState class. */
  object AbstractKaSpaceLinkState {
    @inline def apply(
      orientation: Option[LinkLabel]): AbstractKaSpaceLinkState =
      new AbstractKaSpaceLinkState(orientation)

    /** Empty instance of the AbstractKaSpaceLinkState class. */
    object Empty extends AbstractKaSpaceLinkState(None)
  }


  /** A class to build abstract KaSpace agent states. */
  class PartialAbstractKaSpaceAgentState(val agentType: AgentTypeName)
      extends PartialAbstractKappaLikeAgentState {

    /** Build an abstract agent state with a particular label. */
    @inline final def ~(label: AgentLabel): AbstractAgentState =
      AbstractKaSpaceAgentState(agentType, Some(label))

    @inline final def toAgentState: AgentState =
      AbstractKaSpaceAgentState(agentType, None).toAgentState
  }

  /** Companion object of the PartialAbstractKaSpaceAgentState class. */
  object PartialAbstractKaSpaceAgentState {
    @inline def apply(agentType: AgentTypeName)
        : PartialAbstractKaSpaceAgentState =
      new PartialAbstractKaSpaceAgentState(agentType)
  }

  /** A class to build abstract KaSpace site states. */
  class PartialAbstractKaSpaceSiteState(val name: SiteName)
      extends PartialAbstractKappaLikeSiteState {

    /** Build an abstract site state with a particular label. */
    @inline final def ~(label: SiteLabel): AbstractSiteState =
      AbstractKaSpaceSiteState(name, Some(label))

    @inline final def toSiteState(agentStateSet: AgentStateSet): SiteState =
      AbstractKaSpaceSiteState(name, None).toSiteState(agentStateSet)
  }

  /** Companion object of the PartialAbstractKaSpaceSiteState class. */
  object PartialAbstractKaSpaceSiteState {
    @inline def apply(name: SiteName): PartialAbstractKaSpaceSiteState =
      new PartialAbstractKaSpaceSiteState(name)
  }

  /** A class representing abstract KaSpaceLike links. */
  class AbstractKaSpaceLinked(val id: LinkId,
    val state: AbstractLinkState) extends AbstractLinked

  /** Companion object of the PartialAbstractKaSpaceLinkState class. */
  object AbstractKaSpaceLinked {
    @inline def apply(
      id: LinkId, state: AbstractLinkState): AbstractKaSpaceLinked =
      new AbstractKaSpaceLinked(id, state)
  }

  /** A class to build abstract KaSpaceLike links. */
  class PartialAbstractKaSpaceLinked(val id: LinkId)
      extends AbstractLinked {

    /** Build an abstract link state with a particular label. */
    @inline final def ~(label: LinkLabel): AbstractLinked =
      AbstractKaSpaceLinked(id, AbstractKaSpaceLinkState(Some(label)))

    @inline final def state = AbstractKaSpaceLinkState.Empty
  }

  /** Companion object of the PartialAbstractKaSpaceLinked class. */
  object PartialAbstractKaSpaceLinked {
    @inline def apply(id: LinkId): PartialAbstractKaSpaceLinked =
      new PartialAbstractKaSpaceLinked(id)
  }


  // -- Sugar for pattern and mixture construction. --

  /** Convert site names into abstract site states. */
  implicit def siteNameToAbstractSiteState(
    n: SiteName): PartialAbstractKaSpaceSiteState =
    PartialAbstractKaSpaceSiteState(n)

  /** Convert link IDs into abstract links. */
  implicit def linkIdToAbstractLinked(
    id: LinkId): PartialAbstractKaSpaceLinked =
    PartialAbstractKaSpaceLinked(id)


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
      extends PartialAbstractKaSpaceAgentState(agentTypeName)

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
    def apply(n: SiteName) = PartialAbstractKaSpaceSiteState(n)
  }
}

