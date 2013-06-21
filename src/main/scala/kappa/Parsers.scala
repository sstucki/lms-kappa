package kappa

import scala.language.implicitConversions

import scala.util.parsing.combinator._


trait Parsers {
  this: LanguageContext with AbstractSyntax =>

  // FIXME: Refactor and remove
  /** A trait for anything that is parsed as a link . */
  trait GenericLinkStateSetName {
    /** Unique identifier for a link. */
    def id: LinkId
  }

  // FIXME: Refactor and remove
  // States for contact graphs
  type AgentStateSetName
  type  SiteStateSetName
  type  LinkStateSetName <: GenericLinkStateSetName

  // FIXME: Refactor and remove
  object AST {
    type ContactGraph = List[CGAgent]

    type CGIntf = List[CGSite]

    case class CGAgent(val states: AgentStateSetName,
      val intf: CGIntf)

    case class CGSite(val states: SiteStateSetName,
      val links: List[LinkStateSetName])
  }


  /** Generic parser for the Kappa-like languages. */
  trait GenericParser extends JavaTokenParsers {

    lazy val linkId: Parser[LinkId] = wholeNumber ^^ (_.toInt)

    def agentState: Parser[AbstractAgentState]
    def  siteState: Parser[AbstractSiteState]
    def  linkState: Parser[AbstractLinkState]
    def     linked: Parser[AbstractLinked]

    // -- Patterns (site graphs) --

    // pattern ::= agent [, expr]
    lazy val pattern: Parser[AbstractPattern] = repsep(agent, ",") ^^ {
      as => as.foldLeft(AbstractPattern())(_:+_)
    }

    // agent ::= agent_state [ interface ]
    lazy val agent: Parser[AbstractAgent] = agentState ~ opt(interface) ^^ {
      case state ~ intf => AbstractAgent(state, intf getOrElse Nil)
    }

    // interface ::= "(" [ site ["," interface] ] ")"
    lazy val interface: Parser[List[AbstractSite]] =
      "(" ~> repsep(site, ",") <~ ")"

    // site ::= site_state link
    lazy val site: Parser[AbstractSite] = siteState ~ opt(link) ^^ {
      case state ~ link => AbstractSite(state, link getOrElse AbstractStub) }

    // site ::= "?" | "!" ( wildcard | linked )
    lazy val link: Parser[AbstractLink] =
      undefined ^^ (_ => AbstractUndefined) |
      linkDelim ~> (wildcard | linked) |
      endpointDelim ~> endpoint

    lazy val endpoint: Parser[AbstractEndpoint] = ident ^^ (AbstractEndpoint(_))

    // wildcard ::= "_"
    //   | agent_state [ "." ( "_" | site_state [ "." ( "_" | link_state ) ] ) ]
    lazy val wildcard: Parser[AbstractWildcard] =
      "_" ^^ (_ => AbstractWildcard(None, None, None)) | agentState ~ opt(
        "." ~> ("_" ^^ (_ => None) | ((siteState ^^ (Some(_))) ~ opt(
          "." ~> ("_" ^^ (_ => None) | (linkState ^^ (Some(_)))))) ^^
          (Some(_)))) ^^ {
        case as ~ rest => rest.flatten match {
          case Some(ss ~ ls) => AbstractWildcard(Some(as), ss, ls.flatten)
          case None          => AbstractWildcard(Some(as), None, None)
        }
      }


    // -- Contact graphs -- FIXME: Refactor

    def agentStateSet: Parser[AgentStateSetName]
    def  siteStateSet: Parser[ SiteStateSetName]
    def  linkStateSet: Parser[ LinkStateSetName]

    import AST._

    lazy val cg: Parser[ContactGraph] = repsep(cgAgent, ",")

    lazy val cgAgent: Parser[CGAgent] = agentStateSet ~ opt(cgIntf) ^^ {
      case states ~ intf => CGAgent(states, intf getOrElse List()) }

    lazy val cgIntf: Parser[CGIntf] = "(" ~> repsep(cgSite, ",") <~ ")"

    lazy val cgSite: Parser[CGSite] =
      siteStateSet ~ opt(linkDelim ~> list(linkStateSet)) ^^ {
        case states ~ links => CGSite(states, links getOrElse List()) }

    @inline final protected def list[T](p: Parser[T]) = "{" ~> rep1sep(p, ",") <~ "}"

    def simpleParse[T](p: Parser[T], s: String, name: String) = parseAll(p, s) match {
      case Success(ast, _) => ast
      case msg => println(msg); println();
        throw new IllegalArgumentException(
          "given " + name + " is invalid: " + s)
    }
  }

  val parser: GenericParser

  def parseSiteGraph(s: String): AbstractPattern =
    parser.simpleParse(parser.pattern, s, "site graph")
  def parseContactGraph(s: String): AST.ContactGraph =
    parser.simpleParse(parser.cg, s, "contact graph")

  // FIXME: This implicit is problematic
  implicit def stringToAbstractPattern(s: String): AbstractPattern =
    parseSiteGraph(s)
}

