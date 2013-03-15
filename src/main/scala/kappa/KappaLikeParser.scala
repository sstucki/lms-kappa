package kappa

import scala.util.parsing.combinator._


/** Parser for Kappa-like languages. */
trait KappaLikeParser extends Parser {
  this: KappaLikeContext =>

  lazy val agentType: Parser[AgentTypeName] = ident
  lazy val siteName : Parser[SiteName]      = ident
  lazy val linkName : Parser[LinkName]      = wholeNumber ^^ (_.toInt)

  def agentLabel: Parser[AgentLabel]
  def siteLabel: Parser[SiteLabel]
  def linkLabel: Parser[LinkLabel]

  // States for site graphs
  type AgentStateName = KappaLikeAgentStateName
  type SiteStateName = KappaLikeSiteStateName
  type LinkStateName = KappaLikeLinkStateName

  case class KappaLikeAgentStateName(val agentType: AgentTypeName,
                                     val label: Option[AgentLabel])
  case class KappaLikeSiteStateName(val siteName: SiteName,
                                    val label: Option[SiteLabel])
  case class KappaLikeLinkStateName(val linkName: Option[LinkName],
                                    val label: Option[LinkLabel])
      extends GenericLinkStateName
  {
    @inline final def id: LinkId = linkName getOrElse (throw new IllegalStateException(
      "link doesn't have an id. Possible cause: you forgot to give a bond label"))
  }

  lazy val agentState: Parser[AgentStateName] =
    agentType ~ opt(":" ~> agentLabel) ^^ {
      case atype ~ label => KappaLikeAgentStateName(atype, label) }

  lazy val siteState: Parser[SiteStateName] =
    siteName ~ opt(":" ~> siteLabel) ^^ {
      case sname ~ label => KappaLikeSiteStateName(sname, label) }

  lazy val linkState: Parser[LinkStateName] =
    linkName ~ opt(":" ~> linkLabel) ^^ {
      case lname ~ label =>
        KappaLikeLinkStateName(Some(lname), label) } |
    linkLabel ^^ (label => KappaLikeLinkStateName(None, Some(label)))


  // States for contact graphs
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

  lazy val agentStateSet: Parser[AgentStateSetName] =
    agentType ~ opt(":" ~> list(agentLabel)) ^^ {
      case atype ~ labels =>
        KappaLikeAgentStateSetName(atype, labels getOrElse List()) }

  lazy val siteStateSet: Parser[SiteStateSetName] =
    siteName ~ opt(":" ~> list(siteLabel)) ^^ {
      case sname ~ labels =>
        KappaLikeSiteStateSetName(sname, labels getOrElse List()) }

  lazy val linkStateSet: Parser[LinkStateSetName] =
    linkName ~ opt(":" ~> list(linkLabel)) ^^ {
      case ln ~ labels =>
        KappaLikeLinkStateSetName(ln, labels getOrElse List()) }
}

