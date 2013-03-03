package kappa

import scala.util.parsing.combinator._

/**
 * Parser for the Kappa language
 */
trait Parser extends JavaTokenParsers {
  this: LanguageContext =>

  type LinkId = Int

  // RHZ: Admittedly it's a bit bizarre that link states contain
  // bond labels now. What do you think Sandro?

  /** A trait for anything that is parsed as a link state. */
  trait GenericLinkStateName {
    /** Unique identifier for a link. */
    def id: LinkId
  }

  /** A trait for anything that is parsed as a link . */
  trait GenericLinkStateSetName {
    /** Unique identifier for a link. */
    def id: LinkId
  }

  // States for site graphs
  type AgentStateName
  type  SiteStateName
  type  LinkStateName <: GenericLinkStateName

  val agentState: Parser[AgentStateName]
  val  siteState: Parser[ SiteStateName]
  val  linkState: Parser[ LinkStateName]

  // States for contact graphs
  type AgentStateSetName
  type  SiteStateSetName
  type  LinkStateSetName <: GenericLinkStateSetName

  val agentStateSet: Parser[AgentStateSetName]
  val  siteStateSet: Parser[ SiteStateSetName]
  val  linkStateSet: Parser[ LinkStateSetName]

  object AST {

    /* Site graphs */
    type Expr = List[Agent]

    case class Agent(val state: AgentStateName,
                     val intf: Intf)

    type Intf = List[Site]

    case class Site(state: SiteStateName, link: Link)

    abstract class Link
    case object Undefined extends Link
    case object Stub extends Link
    case class Wildcard(val agentState: Option[AgentStateName],
                        val  siteState: Option[ SiteStateName],
                        val  linkState: Option[ LinkStateName])
      extends Link
    case class Linked(val state: LinkStateName)
      extends Link

    // expr : agent [, expr]
    lazy val expr: Parser[Expr] = repsep(agent, ",")

    // agent : agent_name [: agent_state] [( interface )]
    lazy val agent: Parser[Agent] = agentState ~ opt(interface) ^^ {
      case state ~ intf => Agent(state, intf getOrElse List()) }

    // interface : [ site [, interface] ]
    lazy val interface: Parser[Intf] = "(" ~> repsep(site, ",") <~ ")"

    // site : site_name | site : site_state | site ! bond_label
    lazy val site: Parser[Site] = siteState ~ opt(link) ^^ {
      case state ~ link => Site(state, link getOrElse Stub) }

    lazy val link: Parser[Link] =
      "?" ^^ (_ => Undefined) |
      "!" ~> (wildcard | linkState ^^ (Linked(_)))

    // wildcard : [agentType:agentState | _][.[siteName:siteState | _][.[linkState | _]]]
    lazy val wildcard: Parser[Wildcard] =
      ("_" ^^ (_ => None) | agentState ^^ (Some(_))) ~ opt(
        ("." ~> ("_" ^^ (_ => None) | siteState ^^ (Some(_)))) ~ opt(
        ("." ~> ("_" ^^ (_ => None) | linkState ^^ (Some(_)))))) ^^ {
          case astate ~ Some(sstate ~ lstate) => Wildcard(astate, sstate, lstate.flatten)
          case astate ~ None => Wildcard(astate, None, None)
        }


    /* Contact graphs */
    type ContactGraph = List[CGAgent]

    case class CGAgent(val states: AgentStateSetName,
                       val intf: CGIntf)

    type CGIntf = List[CGSite]

    case class CGSite(val states: SiteStateSetName,
                      val links: List[LinkStateSetName])

    lazy val cg: Parser[ContactGraph] = repsep(cgAgent, ",")

    lazy val cgAgent: Parser[CGAgent] = agentStateSet ~
                                        opt(cgIntf) ^^ {
      case states ~ intf => CGAgent(states, intf getOrElse List()) }

    lazy val cgIntf: Parser[CGIntf] = "(" ~> repsep(cgSite, ",") <~ ")"

    lazy val cgSite: Parser[CGSite] =
      siteStateSet ~ opt("!" ~> list(linkStateSet)) ^^ {
        case states ~ links => CGSite(states, links getOrElse List()) }
  }

  @inline final protected def list[T](p: Parser[T]) = "{" ~> rep1sep(p, ",") <~ "}"

  def simpleParse[T](p: Parser[T], s: String, name: String) = parseAll(p, s) match {
    case Success(ast, _) => ast
    case msg => println(msg); println();
                throw new IllegalArgumentException(
                  "given " + name + " is invalid: " + s)
  }

  def parseSiteGraph(s: String) = simpleParse(AST.expr, s, "site graph")
  def parseContactGraph(s: String) = simpleParse(AST.cg, s, "contact graph")
}

trait KappaLikeParser extends Parser {
  this: LanguageContext =>

  type AgentType = String
  type SiteName = String
  type LinkName = Int // previously called BondLabel

  lazy val agentType: Parser[AgentType] = ident
  lazy val siteName : Parser[SiteName]  = ident
  lazy val linkName : Parser[LinkName]  = wholeNumber ^^ (_.toInt)


  // States for site graphs
  type AgentLabel
  type SiteLabel
  type LinkLabel

  val agentLabel: Parser[AgentLabel]
  val siteLabel: Parser[SiteLabel]
  val linkLabel: Parser[LinkLabel]

  type AgentStateName = KappaLikeAgentStateName
  type SiteStateName = KappaLikeSiteStateName
  type LinkStateName = KappaLikeLinkStateName

  case class KappaLikeAgentStateName(val agentType: AgentType,
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

  case class KappaLikeAgentStateSetName(val agentType: AgentType,
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

