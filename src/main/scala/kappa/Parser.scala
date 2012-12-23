package kappa

import scala.language.postfixOps
import scala.util.parsing.combinator._
import java.io.FileReader

/**
 * Parser for the Kappa language
 */
class Parser extends JavaTokenParsers {
  // TODO make {Link,Agent,Site}State abstract types?
  type LinkState  = String
  type AgentState = String
  type SiteState  = String

  /* Kappa expressions */
  abstract class ExprTerm
  case class LinkAnnot(lnk: BondLabel, state: LinkState) extends ExprTerm
  case class Agent(name: String, state: Option[AgentState], intf: Intf) extends ExprTerm

  type Expr = List[ExprTerm]
  type Intf = List[Site]

  case class Site(name: String, int: Option[SiteState], lnk: Option[BondLabel])
  type BondLabel = Int

  // expr : agent [, expr]
  lazy val expr : Parser[Expr] = rep1sep(agent | lnkState, ",")

  lazy val state = ":" ~> ident

  lazy val lnkState : Parser[LinkAnnot] = wholeNumber ~ state ^^
    { case bondLabel ~ state => LinkAnnot(bondLabel.toInt, state) }

  // agent : agent_name [: agent_state] [( interface )]
  lazy val agent : Parser[Agent] = ident ~ opt(state) ~ opt(interface) ^^
    { case name ~ state ~ intf => Agent(name, state, intf getOrElse List()) }

  // interface : [ site [, interface] ]
  lazy val interface : Parser[Intf] = "(" ~> repsep(site, ",") <~ ")"

  // site : site_name [: site_state] [! bond_label]
  lazy val site : Parser[Site] = ident ~ opt(state) ~ opt("!" ~> bondLabel) ^^
    { case name ~ state ~ bondLabel => Site(name, state, bondLabel) }

  lazy val bondLabel : Parser[BondLabel] = wholeNumber ^^ (_.toInt)

  /* Algebraic expressions
   * NB This is where having an embedded DSL pays off
   */
  abstract class AlgExpr
  case class +(a: AlgExpr, b: AlgExpr) extends AlgExpr
  case class -(a: AlgExpr, b: AlgExpr) extends AlgExpr
  case class *(a: AlgExpr, b: AlgExpr) extends AlgExpr
  case class /(a: AlgExpr, b: AlgExpr) extends AlgExpr
  case class NumericLit(n: Double) extends AlgExpr

  lazy val algExpr : Parser[AlgExpr] = decimalNumber ^^ (x => NumericLit(x.toDouble)) // FIXME dummy

  /* Contact graph */
  abstract class CGTerm
  case class CGAgent(name: String, states: List[AgentState], intf: CGIntf) extends CGTerm
  case class CGLinkAnnot(lnk: BondLabel, states: List[LinkState]) extends CGTerm

  type CGIntf = List[CGSite]
  case class CGSite(name: String, ints: List[SiteState], lnks: List[Int])

  def list[T,U](p: Parser[T], q: Parser[U]) = p ~> "{" ~> (rep1sep(q, ",") <~ "}")
  lazy val states = list(":", ident)
  lazy val lnks = list("!", bondLabel)

  lazy val cgLnkState : Parser[CGLinkAnnot] = wholeNumber ~ states ^^
    { case bondLabel ~ states => CGLinkAnnot(bondLabel.toInt, states) }

  lazy val cgAgent : Parser[CGAgent] = ident ~ opt(states) ~ opt(cgIntf) ^^
    { case name ~ states ~ intf => CGAgent(name, states getOrElse List(), intf getOrElse List()) }

  lazy val cgIntf : Parser[CGIntf] = "(" ~> repsep(cgSite, ",") <~ ")"

  lazy val cgSite : Parser[CGSite] = ident ~ opt(states) ~ opt(lnks) ^^
    { case name ~ states ~ bondLabels => CGSite(name, states getOrElse List(), bondLabels getOrElse List()) }

  /* Kappa file */
  abstract class Decl
  case class ContactGraph(cm: List[CGTerm]) extends Decl
  case class Rule(name: VarName, lhs: Expr, rhs: Expr, rate: AlgExpr) extends Decl
  case class Init(num: Int, expr: Expr) extends Decl

  abstract class Var extends Decl
  case class KappaVar(name: VarName, expr: Expr) extends Var
  case class AlgVar(name: VarName, arith: AlgExpr) extends Var

  case class Plot(name: VarName) extends Decl
  case class Obs(name: VarName, expr: Expr) extends Decl
  case class Mod(bool: BoolExpr, effects: List[Effect], until: Option[BoolExpr]) extends Decl

  type VarName = String

  lazy val kappaFile : Parser[List[Decl]] = rep(decl)

  lazy val decl : Parser[Decl] =
    ( cg
    | rule
    | init
    | kappaVar
    | arithVar
    | plot
    | obs
    | mod
    )

  lazy val cg : Parser[ContactGraph] = "%contact-graph:" ~> repsep(cgAgent | cgLnkState, ",") ^^ (ContactGraph(_))

  lazy val varName : Parser[VarName] = "'[^']+'".r

  lazy val rule : Parser[Rule] = varName ~ expr ~ ("->" ~> expr) ~ ("@" ~> algExpr) ^^
    { case name ~ lhs ~ rhs ~ rate => Rule(name, lhs, rhs, rate) }

  lazy val init : Parser[Init] = "%init:" ~> wholeNumber ~ expr ^^
    { case num ~ expr => Init(num.toInt, expr) }

  lazy val kappaVar : Parser[KappaVar] = "%var:" ~> varName ~ expr ^^
    { case name ~ expr => KappaVar(name, expr) }

  lazy val arithVar : Parser[AlgVar] = "%var:" ~> varName ~ algExpr ^^
    { case name ~ alg => AlgVar(name, alg) }

  lazy val plot : Parser[Plot] = "%plot:" ~> varName ^^ (Plot(_))

  lazy val obs : Parser[Obs] = "%obs:" ~> varName ~ expr ^^
    { case name ~ expr => Obs(name, expr) }

  lazy val mod : Parser[Mod] = "%mod:" ~> boolExpr ~ ("do" ~> rep1sep(effect, ";")) ~ opt(boolExpr) ^^
    { case bool ~ effects ~ until => Mod(bool, effects, until) }

  abstract class BoolExpr
  case class And(a: BoolExpr, b: BoolExpr) extends BoolExpr
  case class Or (a: BoolExpr, b: BoolExpr) extends BoolExpr
  case class Not(a: BoolExpr) extends BoolExpr
  case class Eq (a: AlgExpr, b: AlgExpr) extends BoolExpr
  case class Gt (a: AlgExpr, b: AlgExpr) extends BoolExpr
  case class Lt (a: AlgExpr, b: AlgExpr) extends BoolExpr
  case class Geq(a: AlgExpr, b: AlgExpr) extends BoolExpr
  case class Leq(a: AlgExpr, b: AlgExpr) extends BoolExpr
  case object True  extends BoolExpr
  case object False extends BoolExpr

  lazy val boolExpr : Parser[BoolExpr] =
    ( "[true]"  ^^ { _ => True }
    | "[false]" ^^ { _ => False }
    | "[not]" ~> boolExpr ^^ (Not(_))
    | boolExpr ~ ("&&" ~> boolExpr) ^^ { case a ~ b => And(a, b) }
    | boolExpr ~ ("||" ~> boolExpr) ^^ { case a ~ b => Or (a, b) }
    ) // TODO implement Eq, Gt, Lt, Geq, Leq

  abstract class Effect
  case class Add(alg: AlgExpr, expr: Expr) extends Effect
  case class Del(alg: AlgExpr, expr: Expr) extends Effect
  case class Snapshot(filename: Option[Filename]) extends Effect
  case class Stop(filename: Option[Filename]) extends Effect
  case class Update(varname: VarName, alg: AlgExpr) extends Effect
  type Filename = String

  lazy val effect : Parser[Effect] =
    ( "$ADD" ~> algExpr ~ expr ^^ { case alg ~ expr => Add(alg, expr) }
    | "$DEL" ~> algExpr ~ expr ^^ { case alg ~ expr => Del(alg, expr) }
    | "$UPDATE" ~> varName ~ algExpr ^^ { case varname ~ alg => Update(varname, alg) }
    | "$SNAPSHOT" ~> opt(stringLiteral) ^^ (Snapshot(_))
    | "$STOP" ~> opt(stringLiteral) ^^ (Stop(_))
    )
}

object ParseFile extends Parser {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(kappaFile, reader))
  }
}
