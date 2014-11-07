package kappa


trait KappaParsers extends KappaLikeParsers {
  this: KappaContext
      with KappaAbstractSyntax
      with ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures =>

  object KappaParser extends KappaLikeParser {

    // -- Parsers for labels --

    lazy val agentLabel: Parser[AgentLabel] =
      failure("agent states are not allowed in Kappa")

    lazy val siteLabel: Parser[SiteLabel] = """\w+""".r

    lazy val linkLabel: Parser[LinkLabel] =
      failure("link states are not allowed in Kappa")


    // -- Parsers for site graph states --

    lazy val agentState: Parser[AbstractAgentState] =
      agentName ^^ (AbstractKappaAgentState(_))

    lazy val siteState: Parser[AbstractSiteState] =
      siteName ~ opt(stateDelim ~> siteLabel) ^^ {
        case sname ~ label => AbstractKappaSiteState(sname, label) }

    lazy val linkState: Parser[AbstractLinkState] =
      failure("link states are not allowed in Kappa")

    lazy val linked: Parser[AbstractLinked] =
      linkId ^^ (AbstractKappaLinked(_))


    // -- Parsers for contact graph states --

    lazy val agentStateSet: Parser[AbstractAgentStateSet] =
      agentName ^^ (AbstractKappaAgentStateSet(_))

    lazy val siteStateSet: Parser[AbstractSiteStateSet] =
      siteName ~ opt(stateDelim ~> list(siteLabel)) ^^ {
        case sname ~ states => AbstractKappaSiteStateSet(
          sname, states getOrElse List()) }

    lazy val linkStateSet: Parser[AbstractLinkStateSet] =
      success(AbstractKappaLinkStateSet())
  }

  val parser: GenericParser = KappaParser
}

