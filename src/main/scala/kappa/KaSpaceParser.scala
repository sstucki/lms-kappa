package kappa

import scala.language.implicitConversions

trait KaSpaceParser extends Parser {
  this: KaSpaceContext with Patterns =>

  lazy val agentType: Parser[AgentType] = ident
  lazy val siteName : Parser[SiteName]  = ident

  lazy val float: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def vector3d[T](p: Parser[T]): Parser[Vector[T]] =
    "[" ~> p ~ ("," ~> p) ~ ("," ~> p) <~ "]" ^^ {
      case x ~ y ~ z => Vector(x, y, z) }

  lazy val agentState: Parser[AgentStateName] = float
  lazy val siteState : Parser[ SiteStateName] = vector3d(float) ^^ (xs => Position(xs(0), xs(1), xs(2)))
  lazy val linkState : Parser[ LinkStateName] = vector3d(vector3d(float)) ^^ (xs => Orientation(xs.flatten))
}

