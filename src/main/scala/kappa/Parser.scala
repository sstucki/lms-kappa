package kappa

import scala.util.parsing.combinator._

/**
 * Parser for the Kappa language
 */
trait Parser extends LanguageContext with JavaTokenParsers {
  val agentState : Parser[AgentState]
  val siteState : Parser[SiteState]
  val linkState : Parser[LinkState]

  object AST {
    /* Site graphs */
    abstract class Term
    case class LinkAnnot(lnk: BondLabel, state: LinkState) extends Term
    case class Agent(name: String, state: Option[AgentState], intf: Intf) extends Term

    type Expr = List[Term]
    type Intf = List[Site]

    case class Site(name: String, int: Option[SiteState], lnk: Link)

    abstract class Link
    case object Stub extends Link
    case object Wildcard extends Link
    case object Undefined extends Link
    case class Linked(label: BondLabel) extends Link
    type BondLabel = Int

    // expr : agent [, expr]
    lazy val expr : Parser[Expr] = repsep(agent | linkAnnot, ",")

    lazy val linkAnnot : Parser[LinkAnnot] = bondLabel ~ (":" ~> linkState) ^^
      { case bondLabel ~ state => LinkAnnot(bondLabel, state) }

    // agent : agent_name [: agent_state] [( interface )]
    lazy val agent : Parser[Agent] = ident ~ opt(":" ~> agentState) ~ opt(interface) ^^
      { case name ~ state ~ intf => Agent(name, state, intf getOrElse List()) }

    // interface : [ site [, interface] ]
    lazy val interface : Parser[Intf] = "(" ~> repsep(site, ",") <~ ")"

    // site : site_name | site : site_state | site ! bond_label
    lazy val site : Parser[Site] = ident ^^ (Site(_, None, Undefined)) |
                                   site ~ (":" ~> siteState) ^^ { case Site(name, _, link) ~ state => Site(name, Some(state), link) } |
                                   site ~ link  ^^ { case Site(name, state, _) ~ link => Site(name, state, link) }

    lazy val link : Parser[Link] = "!" ~> (bondLabel ^^ (Linked(_)) |
                                           "_" ^^ (_ => Wildcard))  |
                                   "?" ^^ (_ => Undefined)

    lazy val bondLabel : Parser[BondLabel] = wholeNumber ^^ (_.toInt)


    /* Contact graphs */
    type ContactGraph = List[CGTerm]

    abstract class CGTerm
    case class CGAgent(name: String, states: List[AgentState], intf: CGIntf) extends CGTerm
    case class CGLinkAnnot(lnk: BondLabel, states: List[LinkState]) extends CGTerm

    type CGIntf = List[CGSite]
    case class CGSite(name: String, ints: List[SiteState], lnks: List[BondLabel])

    def list[T](p: Parser[T]) = "{" ~> rep1sep(p, ",") <~ "}"

    lazy val cgLinkAnnot : Parser[CGLinkAnnot] = bondLabel ~ (":" ~> list(linkState)) ^^
      { case bondLabel ~ states => CGLinkAnnot(bondLabel, states) }

    lazy val cgAgent : Parser[CGAgent] = ident ~ opt(":" ~> list(agentState)) ~ opt(cgIntf) ^^
      { case name ~ states ~ intf => CGAgent(name, states getOrElse List(), intf getOrElse List()) }

    lazy val cgIntf : Parser[CGIntf] = "(" ~> repsep(cgSite, ",") <~ ")"

    lazy val cgSite : Parser[CGSite] = ident ^^ (CGSite(_, List(), List())) |
                                       cgSite ~ (":" ~> list(siteState)) ^^ {
                                         case CGSite(name, states1, links) ~ states2 => CGSite(name, states1 ++ states2, links) } |
                                       cgSite ~ ("!" ~> list(bondLabel)) ^^ {
                                         case CGSite(name, states, links1) ~ links2 => CGSite(name, states, links1 ++ links2) }

    lazy val cg : Parser[ContactGraph] = repsep(cgAgent | cgLinkAnnot, ",")
  }

  def parseSiteGraph(s: String) = parseAll(AST.expr, s)
  def parseContactGraph(s: String) = parseAll(AST.cg, s)
}


trait KappaParser extends Parser with KappaContext {
  // agentState and linkState are not really part of core Kappa
  lazy val agentState : Parser[AgentState] = ident
  lazy val siteState : Parser[SiteState] = ident
  lazy val linkState : Parser[LinkState] = ident
}


trait KaSpaceParser extends Parser with KaSpaceContext {
  lazy val decimal : Parser[Double] = decimalNumber ^^ (_.toDouble)
  lazy val agentState : Parser[AgentState] = decimal
  lazy val vec3 = "[" ~> decimal ~ ("," ~> decimal) ~ ("," ~> decimal) <~ "]"
  lazy val siteState : Parser[SiteState] = vec3 ^^ { case x ~ y ~ z => Position(x, y, z) }
  lazy val linkState : Parser[LinkState] = "[" ~> vec3 ~ ("," ~>  vec3) ~ ("," ~> vec3) <~ "]" ^^ {
    case (x1 ~ x2 ~ x3) ~ (y1 ~ y2 ~ y3) ~ (z1 ~ z2 ~ z3) =>
      Vector(Vector(x1, y1, z1), Vector(x2, y2, z2), Vector(x3, y3, z3)) }
}

