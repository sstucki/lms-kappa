package kappa

trait KappaParsers extends KappaLikeParsers {
  this: KappaContext with KappaAbstractSyntax =>

  object KappaParser extends KappaLikeParser {

    lazy val agentLabel: Parser[AgentLabel] =
      failure("agent states are not allowed in Kappa")

    lazy val siteLabel: Parser[SiteLabel] = """\w+""".r

    lazy val linkLabel: Parser[LinkLabel] =
      failure("link states are not allowed in Kappa")

    lazy val agentState: Parser[AbstractAgentState] =
      agentType ^^ (AbstractKappaAgentState(_))

    lazy val siteState: Parser[AbstractSiteState] =
      siteName ~ opt(":" ~> siteLabel) ^^ {
        case sname ~ label => AbstractKappaSiteState(sname, label) }

    lazy val linkState: Parser[AbstractLinkState] =
      failure("link states are not allowed in Kappa")

    lazy val linked: Parser[AbstractLinked] =
      linkId ^^ (AbstractKappaLinked(_))
  }

  val parser: GenericParser = KappaParser
}

