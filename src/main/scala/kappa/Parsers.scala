package kappa

import scala.language.implicitConversions

import scala.util.parsing.combinator._
import scala.collection.mutable


trait Parsers {
  this: LanguageContext
      with ContactGraphs
      with SiteGraphs
      with AbstractSyntax
      with Patterns
      with Mixtures =>

  /** Generic parser for the Kappa-like languages. */
  trait GenericParser extends JavaTokenParsers {

    lazy val linkId: Parser[LinkId] = wholeNumber ^^ (_.toInt)

    lazy val pattern: Parser[AbstractPattern] =
      failure("missing implementation")

    /*
    def agentState: Parser[AbstractAgentState]
    def siteState: Parser[AbstractAgentState#AbstractSiteState]
    def linkState: Parser[AbstractLinkState]
    def linked: Parser[AbstractLinked]

    // -- Patterns (site graphs) --

    // pattern ::= agent [, pattern]
    lazy val pattern: Parser[AbstractPattern] = repsep(agent, ",") ^^ {
      as => as.foldLeft(AbstractPattern())(_:+_)
    }

    // agent ::= agent_state [ interface ]
    lazy val agent: Parser[AbstractAgent] = agentState ~ opt(interface) ^^ {
      case state ~ intf => AbstractAgent(state, intf getOrElse Nil)
    }

    // interface ::= "(" [ site ["," interface] ] ")"
    lazy val interface: Parser[Seq[AbstractSite]] =
      "(" ~> repsep(site, ",") <~ ")"

    // site ::= site_state link
    lazy val site: Parser[AbstractSite] = siteState ~ opt(link) ^^ {
      case state ~ link => AbstractSite(state, link getOrElse AbstractStub) }

    // site ::= "?" | "!" ( wildcard | linked )
    lazy val link: Parser[AbstractLink] =
      undefined ^^ (_ => AbstractUndefined) |
      linkDelim ~> (wildcard | linked) |
      endpointDelim ~> endpoint

    lazy val endpoint: Parser[AbstractEndpoint] =
      ident ^^ (AbstractEndpoint(_))

    // wildcard ::= "_" | agent_state [ "." ( "_" |
    //                     site_state [ "." ( "_" | link_state ) ] ) ]
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
    */


    // -- Contact graphs --

    def agentStateSet: Parser[AbstractAgentStateSet]
    def siteStateSet: Parser[AbstractSiteStateSet]
    def linkStateSet: Parser[AbstractLinkStateSet]

    lazy val cg: Parser[AbstractCG] =
      repsep(cgAgent, ",") ^^ { xs => AbstractCG(xs.toVector) }

    lazy val cgAgent: Parser[AbstractCGAgent] =
      agentStateSet ~ opt(cgIntf) ^^ {
        case states ~ intf =>
          AbstractCGAgent(states, intf getOrElse List()) }

    lazy val cgIntf: Parser[Seq[AbstractCGSite]] =
      "(" ~> repsep(cgSite, ",") <~ ")"

    lazy val cgSite: Parser[AbstractCGSite] =
      siteStateSet ~ opt(linkDelim ~> list(cgLink)) ^^ {
        case states ~ links =>
          AbstractCGSite(states, links getOrElse List()) }

    lazy val cgLink: Parser[AbstractCGLink] =
      linkId ~ linkStateSet ^^ {
        case id ~ states => AbstractCGLink(id, states) }

    @inline final protected def list[T](p: Parser[T]) =
      "{" ~> rep1sep(p, ",") <~ "}"


    def simpleParse[T](p: Parser[T], s: String, name: String) =
      parseAll(p, s) match {
        case Success(ast, _) => ast
        case msg => {
          println(msg)
          println()
          throw new IllegalArgumentException(
            "given " + name + " is invalid: " + s)
        }
      }
  }

  val parser: GenericParser

  def parseSiteGraph(s: String): AbstractPattern =
    parser.simpleParse(parser.pattern, s, "site graph")

  def parseContactGraph(s: String): AbstractCG =
    parser.simpleParse(parser.cg, s, "contact graph")

  implicit def stringToAbstractPattern(s: String): AbstractPattern =
    parseSiteGraph(s)

  implicit def stringToContactGraph(s: String): ContactGraph = {

    val ast = parseContactGraph(s)
    val cg = new ContactGraph

    val linkMap = new mutable.ArrayBuffer[(ContactGraph.Site,
      AbstractCGLink)]

    for (AbstractCGAgent(states, sites) <- ast.agents) {
      val u = new ContactGraph.Agent(states.toAgentStateSet)
      cg += u
      for (AbstractCGSite(states, links) <- sites) {
        val x = u += states.toSiteStateSet
        for (l <- links)
          linkMap += (x -> l)
      }
    }

    for (l <- linkMap groupBy { case (_, l) => l.id }) l match {
      case (id, Seq((x1, l1), (x2, l2))) => {
        x1 connect (x2, l1.states.toLinkStateSet)
        x2 connect (x1, l2.states.toLinkStateSet)
      }
      case (id, Seq(_)) => throw new IllegalStateException(
        "dangling link with label " + id)
      case (id, xs) => throw new IllegalStateException(
        "attempt to create hyperlink with label " + id +
          " to connect sites: " + (xs map (_._1)).mkString(", "))
    }

    cg
  }

  // -- String to pattern/mixture conversion --

  /**
   * Build a pattern from a string.
   *
   * This method invokes the [[Parser]] to parse an expression.  It
   * then walks the [[Parser.AST]] and builds a [[Patterns.Pattern]]
   * from the expression using the operators of the abstract syntax
   * classes.
   *
   * @param expr the string to build the pattern from.
   * @return a pattern corresponding to the expression `expr`.
   */
  def stringToPattern(expr: String): Pattern =
    parseSiteGraph(expr).toPattern

  /**
   * Build a mixture from a string.
   *
   * This method first builds a [[Patterns#Pattern]] from a string and
   * subsequently converts it into a [[Mixtures#Mixture]].
   *
   * @param expr the string to build the mixture from.
   * @return a mixture corresponding to the expression `expr`.
   */
  def stringToMixture(expr: String): Mixture =
    parseSiteGraph(expr).toMixture


  // -- String interpolation --

  implicit def interpolator(sc: StringContext): Interpolator =
    new Interpolator(sc)

  final class Interpolator(sc: StringContext) {
    def p(args: Any*): Pattern = stringToPattern( sc.s(args :_*) )
    def m(args: Any*): Mixture = stringToMixture( sc.s(args :_*) )
  }
}

