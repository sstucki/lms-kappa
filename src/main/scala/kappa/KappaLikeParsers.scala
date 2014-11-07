package kappa

import scala.util.parsing.combinator._

trait KappaLikeParsers extends Parsers {
  this: KappaLikeContext
      with KappaLikeAbstractSyntax
      with ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures =>

  /** A parser for Kappa-like languages. */
  trait KappaLikeParser extends GenericParser {

    lazy val agentName: Parser[AgentName] = ident
    lazy val siteName: Parser[SiteName] = ident

    // -- Parsers for labels --

    def agentLabel: Parser[AgentLabel]
    def siteLabel: Parser[SiteLabel]
    def linkLabel: Parser[LinkLabel]
  }
}

