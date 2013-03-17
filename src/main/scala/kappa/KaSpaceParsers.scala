package kappa

import scala.language.implicitConversions

trait KaSpaceParsers extends KappaLikeParsers {
  this: KaSpaceContext with KaSpaceAbstractSyntax =>

  object KaSpaceParser extends KappaLikeParser {

    lazy val agentState: Parser[AbstractAgentState] =
      agentType ~ opt(":" ~> agentLabel) ^^ {
        case atype ~ label => AbstractKaSpaceAgentState(atype, label) }

    lazy val siteState: Parser[AbstractSiteState] =
      siteName ~ opt(":" ~> siteLabel) ^^ {
        case sname ~ label => AbstractKaSpaceSiteState(sname, label) }

    lazy val linkState: Parser[AbstractLinkState] =
      linkLabel ^^ { l => AbstractKaSpaceLinkState(Some(l)) }

    lazy val linked: Parser[AbstractLinked] =
      linkId ~ opt(":" ~> linkState) ^^ {
        case id ~ state => AbstractKaSpaceLinked(
          id, state getOrElse AbstractKaSpaceLinkState(None))
      }


    lazy val float: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

    def vector3d[T](p: Parser[T]): Parser[Vector[T]] =
      "[" ~> p ~ ("," ~> p) ~ ("," ~> p) <~ "]" ^^ {
        case x ~ y ~ z => Vector(x, y, z) }

    lazy val agentLabel: Parser[AgentLabel] = float
    lazy val  siteLabel: Parser[ SiteLabel] = vector3d(float) ^^ (Position(_))
    lazy val  linkLabel: Parser[ LinkLabel] =
      vector3d(vector3d(float)) ^^ (Orientation(_))
  }

  val parser: GenericParser = KaSpaceParser
}

