package kappa

import scala.language.implicitConversions


trait KaSpaceParsers extends KappaLikeParsers {
  this: KaSpaceContext
      with KaSpaceAbstractSyntax
      with ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures =>


  // States for contact graphs
  // TODO: This can't be put in KappaLikeParsers because we don't know
  // there about KaSpaceAgentStateSet, but we should declare the
  // virtual method `def KappaLikeAgentStateSet(agentName: AgentName,
  // states: List[AgentLabel])` in KappaLikeContext... Unnecessary,
  // because these case classes have been defined in K*AbstractSyntax
  //
  // final case class AbstractKaSpaceAgentStateSet(
  //   val agentName: AgentName,
  //   val states: List[AgentLabel])
  //     extends AbstractAgentStateSet {
  //   /** Creates an agent state set from this abstract agent state set. */
  //   def toAgentStateSet = KaSpaceAgentStateSet(agentName, states)
  // }

  // final case class AbstractKaSpaceSiteStateSet(
  //   val siteName: SiteName,
  //   val states: List[SiteLabel])
  //     extends AbstractSiteStateSet {
  //   /** Creates a site state set from this abstract site state set. */
  //   def toSiteStateSet = KaSpaceSiteStateSet(siteName, states)
  // }

  // final case class AbstractKaSpaceLinkStateSet(
  //   val states: List[LinkLabel])
  //     extends AbstractLinkStateSet {
  //   /** Creates a link state set from this abstract link state set. */
  //   def toLinkStateSet = KaSpaceLinkStateSet(states)
  // }


  object KaSpaceParser extends KappaLikeParser {

    // -- Parsers for labels --

    lazy val float: Parser[Double] =
      floatingPointNumber ^^ (_.toDouble)

    def vector3d[T](p: Parser[T]): Parser[Vector[T]] =
      "[" ~> p ~ ("," ~> p) ~ ("," ~> p) <~ "]" ^^ {
        case x ~ y ~ z => Vector(x, y, z) }

    lazy val agentLabel: Parser[AgentLabel] = float

    lazy val siteLabel: Parser[SiteLabel] =
      vector3d(float) ^^ (Position(_))

    lazy val linkLabel: Parser[LinkLabel] =
      vector3d(vector3d(float)) ^^ (Orientation(_))


    // -- Parsers for site graph states --

    lazy val agentState: Parser[AbstractAgentState] =
      agentName ~ opt(stateDelim ~> agentLabel) ^^ {
        case aname ~ label => AbstractKaSpaceAgentState(aname, label) }

    lazy val siteState: Parser[AbstractSiteState] =
      siteName ~ opt(stateDelim ~> siteLabel) ^^ {
        case sname ~ label => AbstractKaSpaceSiteState(sname, label) }

    lazy val linkState: Parser[AbstractLinkState] =
      linkLabel ^^ { l => AbstractKaSpaceLinkState(Some(l)) }

    lazy val linked: Parser[AbstractLinked] =
      linkId ~ opt(stateDelim ~> linkState) ^^ {
        case id ~ state => AbstractKaSpaceLinked(
          id, state getOrElse AbstractKaSpaceLinkState(None))
      }


    // -- Parsers for contact graph states --

    lazy val agentStateSet: Parser[AbstractAgentStateSet] =
      agentName ~ opt(stateDelim ~> list(agentLabel)) ^^ {
        case aname ~ states => AbstractKaSpaceAgentStateSet(
          aname, states getOrElse List()) }

    lazy val siteStateSet: Parser[AbstractSiteStateSet] =
      siteName ~ opt(stateDelim ~> list(siteLabel)) ^^ {
        case sname ~ states => AbstractKaSpaceSiteStateSet(
          sname, states getOrElse List()) }

    lazy val linkStateSet: Parser[AbstractLinkStateSet] =
      opt(stateDelim ~> list(linkLabel)) ^^ { ls =>
        AbstractKaSpaceLinkStateSet(ls getOrElse List()) }
  }

  val parser: GenericParser = KaSpaceParser
}
