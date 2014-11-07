package kappa


trait KappaParsers extends KappaLikeParsers {
  this: KappaContext
      with KappaAbstractSyntax
      with ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures =>


  // final case class AbstractKappaAgentStateSet(
  //   val agentName: AgentName)
  //     extends AbstractAgentStateSet {
  //   /** Creates an agent state set from this abstract agent state set. */
  //   def toAgentStateSet = KappaAgentStateSet(agentName)
  // }

  // final case class AbstractKappaSiteStateSet(
  //   val siteName: SiteName,
  //   val states: List[SiteLabel])
  //     extends AbstractSiteStateSet {
  //   /** Creates a site state set from this abstract site state set. */
  //   def toSiteStateSet = KappaSiteStateSet(siteName, states)
  // }

  // final case class AbstractKappaLinkStateSet()
  //     extends AbstractLinkStateSet {
  //   /** Creates a link state set from this abstract link state set. */
  //   def toLinkStateSet = KappaLinkStateSet()
  // }


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

