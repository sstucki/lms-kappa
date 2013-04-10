package kappa

import scala.util.parsing.combinator._


trait KappaLikeParsers extends Parsers {
  this: KappaLikeContext with KappaLikeAbstractSyntax =>

  // States for contact graphs -- FIXME: Refactor and remove
  type LinkName = LinkId
  type AgentStateSetName = KappaLikeAgentStateSetName
  type SiteStateSetName = KappaLikeSiteStateSetName
  type LinkStateSetName = KappaLikeLinkStateSetName

  case class KappaLikeAgentStateSetName(val agentType: AgentTypeName,
    val labels: List[AgentLabel])
  case class KappaLikeSiteStateSetName(val siteName: SiteName,
    val labels: List[SiteLabel])
  case class KappaLikeLinkStateSetName(val linkName: LinkName,
    val labels: List[LinkLabel])
      extends GenericLinkStateSetName
  {
    @inline final def id: LinkId = linkName
  }

  /** A parser for Kappa-like languages. */
  trait KappaLikeParser extends GenericParser {

    // Parsers for site graph states
    lazy val agentType: Parser[AgentTypeName] = ident
    lazy val siteName : Parser[SiteName]      = ident

    def agentLabel: Parser[AgentLabel]
    def  siteLabel: Parser[ SiteLabel]
    def  linkLabel: Parser[ LinkLabel]

    // Parsers for contact graph states -- FIXME: Refactor
    lazy val agentStateSet: Parser[AgentStateSetName] =
      agentType ~ opt(stateDelim ~> list(agentLabel)) ^^ {
        case atype ~ labels =>
          KappaLikeAgentStateSetName(atype, labels getOrElse List()) }

    lazy val siteStateSet: Parser[SiteStateSetName] =
      siteName ~ opt(stateDelim ~> list(siteLabel)) ^^ {
        case sname ~ labels =>
          KappaLikeSiteStateSetName(sname, labels getOrElse List()) }

    lazy val linkStateSet: Parser[LinkStateSetName] =
      linkId ~ opt(stateDelim ~> list(linkLabel)) ^^ {
        case ln ~ labels =>
          KappaLikeLinkStateSetName(ln, labels getOrElse List()) }
  }
}

