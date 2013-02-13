package kappa

import scala.language.implicitConversions

import breeze.linalg.{Vector => BVector,_}

trait KaSpaceParser extends Parser {
  this: KaSpaceContext with Patterns =>

  lazy val agentType : Parser[AgentType] = ident
  lazy val siteName  : Parser[SiteName]  = ident

  lazy val float: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def vector3d[T](p: Parser[T]): Parser[Vector[T]] =
    "[" ~> p ~ ("," ~> p) ~ ("," ~> p) <~ "]" ^^ {
      case x ~ y ~ z => Vector(x, y, z) }

  lazy val agentState : Parser[AgentStateName] = float
  lazy val siteState  : Parser[SiteStateName]  = vector3d(float) ^^ seqToVec
  lazy val linkState  : Parser[LinkStateName]  = vector3d(vector3d(float)) ^^ seqToMatrix

  def seqToVec(xs: Seq[Double]): DenseVector[Double] = DenseVector(xs.toArray)
  def seqToMatrix(xs: Seq[Seq[Double]]): DenseMatrix[Double] = new DenseMatrix(xs.length, xs.flatten.toArray)
  def vecToSeq[T](v: BVector[T]): Seq[T] = v.valuesIterator.toSeq
  def matrixToSeq[T](m: Matrix[T]): Seq[Seq[T]] = for (i <- 0 until m.rows)
                                                  yield (for (j <- 0 until m.cols)
                                                         yield m(i, j))

  implicit def scToKaSpace(sc: StringContext): ToKaSpace = new ToKaSpace(sc)

  class ToKaSpace(sc: StringContext) {
    def getString[T](x: T): String = x match {
      // FIXME Maybe we lose too much precision by converting
      // Doubles to Strings and then back to Doubles again
      case x: Double => x.toString
      case xs: Seq[_] => "[" + xs.map(getString).mkString(", ") + "]"
      case v: BVector[_] => getString(vecToSeq(v))
      case m:  Matrix[_] => getString(matrixToSeq(m))
    }

    def k(args: Any*): String =
      sc.s( (args map getString) :_*)

    def p(args: Any*): Pattern =
      Pattern( k(args :_*) )
  }
}

