package kappa

import scala.language.implicitConversions

trait KaSpaceParser extends KappaLikeParser {
  this: KaSpaceContext =>

  type AgentLabel = Double
  type  SiteLabel = Position
  type  LinkLabel = Orientation

  lazy val float: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def vector3d[T](p: Parser[T]): Parser[Vector[T]] =
    "[" ~> p ~ ("," ~> p) ~ ("," ~> p) <~ "]" ^^ {
      case x ~ y ~ z => Vector(x, y, z) }

  lazy val agentLabel: Parser[AgentLabel] = float
  lazy val  siteLabel: Parser[ SiteLabel] = vector3d(float) ^^ (Position(_))
  lazy val  linkLabel: Parser[ LinkLabel] =
    vector3d(vector3d(float)) ^^ (Orientation(_))
}

