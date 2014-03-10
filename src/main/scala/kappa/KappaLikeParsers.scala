package kappa

import scala.util.parsing.combinator._

trait KappaLikeParsers extends Parsers {
  this: KappaLikeContext
      with KappaLikeAbstractSyntax
      with ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures =>

  /*
  // States for contact graphs
  type LinkName = LinkId
  type AgentStateSetName = KappaLikeAgentStateSetName
  type SiteStateSetName = KappaLikeSiteStateSetName
  type LinkStateSetName = KappaLikeLinkStateSetName

  final case class AbstractKappaLikeAgentStateSet(
    val agentType: AgentTypeName,
    val states: List[AgentLabel])
      extends AbstractAgentStateSet

  final case class AbstractKappaLikeSiteStateSet(
    val siteName: SiteName,
    val states: List[SiteLabel])
      extends AbstractSiteStateSet

  final case class AbstractKappaLikeLinkStateSet(
    val states: List[LinkLabel])
      extends AbstractLinkStateSet
  // {
  //   @inline final def id: LinkId = linkName
  // }
  */

  /** A parser for Kappa-like languages. */
  trait KappaLikeParser extends GenericParser {

    lazy val agentName: Parser[AgentName] = ident
    lazy val siteName: Parser[SiteName] = ident

    // -- Parsers for labels --

    def agentLabel: Parser[AgentLabel]
    def siteLabel: Parser[SiteLabel]
    def linkLabel: Parser[LinkLabel]
  }
}

