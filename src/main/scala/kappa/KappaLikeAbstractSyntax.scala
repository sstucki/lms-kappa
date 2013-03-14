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
 * [[KappaLikeParser.AST]] classes (or vice-versa) for consistency.
 */
trait KappaLikeAbstractSyntax extends AbstractSyntax {
  this: KappaLikeContext
      with ContactGraphs
      with Patterns
      with Mixtures
      with Actions
      with KappaLikeParser =>


  // -- Nodes of abstract syntax trees and their builders. --

  /** A class representing abstract KappaLike agent states. */
  abstract class AbstractKappaLikeAgentState extends AbstractAgentState {

    /** The agent type of this KappaLike agent state. */
    def atype: AgentType

    /** Find an agent state set in the contact graph. */
    def findAgentStateSet: AgentStateSet =
      contactGraph.agentStateSets find (_.agentType == atype) getOrElse {
        throw new IllegalArgumentException(
          "couldn't find agent state set for \"" + atype + "\"")
      }
  }

  /** A class representing abstract KappaLike site states. */
  abstract class AbstractKappaLikeSiteState extends AbstractSiteState {

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


  /** A class to build abstract KappaLike agent states. */
  abstract class AbstractKappaLikeAgentStateBuilder(val atype: AgentType) {

    @inline def %(label: AgentLabel): AbstractAgentBuilder =
      AbstractAgentBuilder(toAbstractAgentState(Some(label)))
    @inline def apply(sites: AbstractSite*): AbstractAgent =
      toAbstractAgentBuilder.apply(sites: _*)
    @inline def ~(that: AbstractAgent): AbstractPattern =
      toAbstractAgentBuilder.toAbstractAgent ~ that
    @inline def ->(that: Pattern)(implicit ab: ActionBuilder) =
      ab(this.toPattern, that)

    @inline def toAbstractAgentBuilder: AbstractAgentBuilder =
      AbstractAgentBuilder(toAbstractAgentState(None))
    @inline def toPattern: Pattern = toAbstractAgentBuilder.toPattern

    def toAbstractAgentState(label: Option[AgentLabel]): AbstractAgentState
  }

  /** A class to build abstract KappaLike site states. */
  abstract class AbstractKappaLikeSiteStateBuilder(val name: SiteName) {

    @inline def %(label: SiteLabel): AbstractSiteBuilder =
      AbstractSiteBuilder(toAbstractSiteState(Some(label)))
    @inline def ?(): AbstractSite =
      toAbstractSiteBuilder.?
    @inline def !-(): AbstractSite =
      toAbstractSiteBuilder.!-
    @inline def !(name: LinkName, state: AbstractLinkState): AbstractSite =
      toAbstractSiteBuilder.!(name, state)
    @inline def !(linked: AbstractLinked): AbstractSite =
      toAbstractSiteBuilder.!(linked)
    @inline def !(wc: AbstractWildcard): AbstractSite =
      toAbstractSiteBuilder.!(wc)
    @inline def !*(): AbstractSite =
      toAbstractSiteBuilder.!*

    @inline def toAbstractSiteBuilder: AbstractSiteBuilder =
      AbstractSiteBuilder(toAbstractSiteState(None))
    @inline def toAbstractSite: AbstractSite = this.?

    def toAbstractSiteState(label: Option[SiteLabel]): AbstractSiteState
  }

  /** A class to build abstract KappaLike link states. */
  abstract class AbstractKappaLikeLinkStateBuilder(val name: LinkName) {

    @inline def %(label: LinkLabel): AbstractLinked =
      AbstractLinked(name, toAbstractLinkState(Some(label)))

    @inline def toAbstractLinked: AbstractLinked =
      AbstractLinked(name, toAbstractLinkState(None))

    def toAbstractLinkState(label: Option[LinkLabel]): AbstractLinkState
  }


  // -- Sugar for pattern and mixture construction. --

  /** Convert abstract agent state builders into patterns. */
  implicit def abstractAgentStateBuilderToPattern(
    b: AbstractKappaLikeAgentStateBuilder): Pattern = b.toPattern

  /** Convert abstract site state builders into abstract sites. */
  implicit def abstractSiteStateBuilderToAbstractSite(
    b: AbstractKappaLikeSiteStateBuilder): AbstractSite = b.toAbstractSite

  /** Convert abstract link state builders into abstract links. */
  implicit def abstractSiteStateBuilderToAbstractLinked(
    b: AbstractKappaLikeLinkStateBuilder): AbstractLinked = b.toAbstractLinked
}

