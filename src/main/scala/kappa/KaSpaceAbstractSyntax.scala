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
      with SiteGraphs
      with Patterns
      with KaSpaceActions
      with Mixtures
      with Rules =>

  // -- Contact graphs --

  final case class AbstractKaSpaceAgentStateSet(agentName: AgentName,
    radii: List[AgentLabel])
      extends AbstractAgentStateSet {

    def toAgentStateSet: AgentStateSet =
      KaSpaceAgentStateSet(agentName, radii)
  }

  final case class AbstractKaSpaceSiteStateSet(siteName: SiteName,
    positions: List[SiteLabel])
      extends AbstractSiteStateSet {

    def toSiteStateSet: SiteStateSet =
      KaSpaceSiteStateSet(siteName, positions)
  }

  final case class AbstractKaSpaceLinkStateSet(
    orientations: List[LinkLabel])
      extends AbstractLinkStateSet {

    def toLinkStateSet: LinkStateSet =
      KaSpaceLinkStateSet(orientations)
  }


  // -- Agents --

  /** A class representing abstract KaSpace agent states. */
  sealed case class AbstractKaSpaceAgentState(
    agentName: AgentName,
    radius: Option[AgentLabel])
      extends AbstractKappaLikeAgentState {

    @inline final def label = radius

    /** Build an abstract agent state with a particular label. */
    @inline final def ~(radius: AgentLabel): AbstractAgentState =
      AbstractKaSpaceAgentState(agentName, Some(radius))

    /** Creates an agent state from this abstract agent state. */
    @inline final def toAgentState =
      KaSpaceAgentState(agentType.states, radius)
  }

  // -- Sites --

  /** A class representing abstract KaSpace site states. */
  sealed case class AbstractKaSpaceSiteState(
    siteName: SiteName,
    position: Option[SiteLabel])
      extends AbstractKappaLikeSiteState {

    @inline final def label = position

    /** Build an abstract site state with a particular label. */
    @inline final def ~(position: SiteLabel) =
      AbstractKaSpaceSiteState(siteName, Some(position))

    /** Creates a site state from this abstract site state. */
    @inline final def toSiteState(agentType: ContactGraph.Agent) =
      KaSpaceSiteState(siteType(agentType).states, position)
  }

  // - Aliases -

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
  final class AgentType(agentName: AgentName, radius: Option[AgentLabel])
      extends AbstractKaSpaceAgentState(agentName, radius)

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
  // TODO: Should this be an object with an apply method instead, like
  // object Site {
  //   def apply(n: SiteName, position: Option[SiteLabel]) =
  //     AbstractKaSpaceSiteState(siteName, position)
  // }
  // Same applies for AgentType.
  final class Site(siteName: SiteName, position: Option[SiteLabel])
      extends AbstractKaSpaceSiteState(siteName, position)

  // -- Links --

  /** A class representing abstract KaSpace links. */
  final case class AbstractKaSpaceLinked(id: LinkId,
    state: AbstractLinkState = AbstractKaSpaceLinkState(None))
      extends AbstractLinked {

    /** Build an abstract link state with a particular label. */
    @inline final def ~(orientation: LinkLabel): AbstractLinked =
      AbstractKaSpaceLinked(id,
        AbstractKaSpaceLinkState(Some(orientation)))
  }

  /** A class representing abstract KaSpace link states. */
  final case class AbstractKaSpaceLinkState(
    orientation: Option[LinkLabel])
      extends AbstractLinkState {

    /** Creates a link state from this abstract link state. */
    @inline def toLinkState(set: KaSpaceLinkStateSet,
      linkId: Option[LinkId]) =
      KaSpaceLinkState(set, orientation, linkId)
  }

  /** Convert link IDs into abstract links. */
  implicit def linkIdToAbstractLinked(id: LinkId) =
    AbstractKaSpaceLinked(id)
}

