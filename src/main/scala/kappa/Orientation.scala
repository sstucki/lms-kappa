package kappa

import scala.language.implicitConversions

import scala.math.{sin,cos,abs}

/**
 * A class representing orientations in 3D euclidean space.
 *
 * NOTE: The orientation is represented as 3x3 rotation matrix.  The
 * matrix is given in column-major order, i.e. as a set of 3 basis
 * vectors.
 *
 * @param e1 the first (X) basis vector of the rotation matrix.
 * @param e2 the second (Y) basis vector of the rotation matrix.
 * @param e3 the third (Z) basis vector of the rotation matrix.
 */
final class Orientation private (
  val e1: Position, val e2: Position, val e3: Position) extends Equals {

  import DoubleApprox._

  /** Apply this rotation to a position `that`. */
  @inline def *(that: Position): Position =
    this.e1 * that.x1 + this.e2 * that.x2 + this.e3 * that.x3

  /** Combine (multiply) this rotation with another one. */
  @inline def *(that: Orientation): Orientation = Orientation(
    this * that.e1, this * that.e2, this * that.e3)

  /** Return the inverse of this rotation matrix. */
  @inline def inv = t

  /** Return the transpose of this rotation matrix. */
  @inline def t = Orientation(
    Position(e1.x1, e2.x1, e3.x1),
    Position(e1.x2, e2.x2, e3.x2),
    Position(e1.x3, e2.x3, e3.x3))

  /**
   * Compare this rotation matrix (element-wise) to `that` up to a
   * fudge factor.
   *
   * @param that the orientation to compare `this` to.
   * @param epsilon the fudge factor.
   */
  def ~=(that: Orientation, epsilon: Double = DoubleApprox.epsilon): Boolean =
    (this.e1 ~= (that.e1, epsilon)) &&
    (this.e2 ~= (that.e2, epsilon)) &&
    (this.e3 ~= (that.e3, epsilon))

  /**
   * Return the determinant of this rotation matrix.
   *
   * NOTE: This method just returns a constant 1.0.
   */
  @inline def det = 1

  /**
   * Return the actual determinant of the underlying rotation
   * matrix.
   */
  private def actualDet =
    e1.x1 * (e2.x2 * e3.x3 - e3.x2 * e2.x3) +
    e1.x2 * (e3.x1 * e2.x3 - e2.x1 * e3.x3) +
    e1.x3 * (e2.x1 * e3.x2 - e3.x1 * e2.x2)

  /** Check if this matrix is indeed in SO(3). */
  @inline def isConsistent: Boolean = actualDet ~= 1.0


  // -- Equals API --

  @inline def canEqual(that: Any) = that.isInstanceOf[Orientation]


  // -- Any API --

  @inline override def equals(that: Any): Boolean = that match {
    case that: Orientation => if (this eq that) true else {
      (that canEqual this) &&
      (this.e1 == that.e1) &&
      (this.e2 == that.e2) &&
      (this.e3 == that.e3)
    }
    case _ => false
  }

  @inline override def hashCode: Int = e1.## * 41 * e2.## + 1681 * e3.##

  @inline override def toString = "[" + e1 + ", " + e2 + ", " + e3 + "]"
}

/** Companion object of the [[Orientation]] class. */
object Orientation {

  /**
   * Factory method for constructing [[Orientation]]s from 3
   * orthonormal basis vectors.
   *
   * The 3 basis vectors correspond to the column vectors of the
   * rotation matrix.
   *
   * @param e1 the first (X) basis vector of the rotation matrix.
   * @param e2 the second (Y) basis vector of the rotation matrix.
   * @param e3 the third (Z) basis vector of the rotation matrix.
   */
  def apply(e1: Position, e2: Position, e3: Position): Orientation = {
    val o = new Orientation(e1, e2, e3)
    if (o.isConsistent) o else throw new IllegalArgumentException(
      "attempt to create inconsistent rotation matrix, det(" + o +
        ") = " + o.actualDet)
  }

  /**
   * Factory method for constructing [[Orientation]]s from a 3x3
   * rotation matrix given as 9 individual values in row-major
   * order.
   */
  def apply(
    e11: Double, e12: Double, e13: Double,
    e21: Double, e22: Double, e23: Double,
    e31: Double, e32: Double, e33: Double): Orientation = Orientation(
      Position(e11, e21, e31),
      Position(e12, e22, e32),
      Position(e13, e23, e33))

  /**
   * Factory method for constructing [[Orientation]]s from a 3x3
   * rotation matrix given as a sequence of 9 individual values in
   * row-major order.
   *
   * @param m a sequence representing a 3x3 rotation matrix in
   *        row-major order.
   */
  def apply(m: Seq[Double]): Orientation = {
    if (m.length != 9) throw new IllegalArgumentException(
      "attempt to create 3x3 rotation matrix from sequence of " +
        m.length + " values")
    else Orientation(
      m(0), m(1), m(2),
      m(3), m(4), m(5),
      m(6), m(7), m(8))
  }

  /**
   * Factory method for constructing [[Orientation]]s from a given
   * rotation axis and angle.
   *
   * @param a the rotation axis represented as a 3D vector.
   * @param theta the rotation angle in radians.
   */
  def apply(a: Position, theta: Double): Orientation = {
    val c = cos(theta)
    val d = 1 - c
    val s = sin(theta)
    Orientation(
      Position(
        c + a.x1 * a.x1 * d,
        a.x2 * a.x1 * d + a.x3 * s,
        a.x3 * a.x1 * d - a.x2 * s),
      Position(
        a.x1 * a.x2 * d - a.x3 * s,
        c + a.x2 * a.x2 * d,
        a.x3 * a.x2 * d + a.x1 * s),
      Position(
        a.x1 * a.x3 * d + a.x2 * s,
        a.x2 * a.x3 * d - a.x1 * s,
        c + a.x3 * a.x3 * d))
  }

  /** Factory method for the identity rotation matrix. */
  def apply(): Orientation = Orientation(1, 0, 0, 0, 1, 0, 0, 0, 1)

  /**
   * Factory method for constructing [[Orientation]]s around the
   * X-axis from a given an angle.
   *
   * @param theta the rotation angle around the X-axis in radians.
   */
  def x(theta: Double) = {
    val c = cos(theta)
    val s = sin(theta)
    Orientation(
      Position(1, 0, 0), Position(0, c, s), Position(0, -s, c))
  }

  /**
   * Factory method for constructing [[Orientation]]s around the
   * Y-axis from a given an angle.
   *
   * @param theta the rotation angle around the Y-axis in radians.
   */
  def y(theta: Double) = {
    val c = cos(theta)
    val s = sin(theta)
    Orientation(
      Position(c, 0, -s), Position(0, 1, 0), Position(s, 0, c))
  }

  /**
   * Factory method for constructing [[Orientation]]s around the
   * Z-axis from a given an angle.
   *
   * @param theta the rotation angle around the Z-axis in radians.
   */
  def z(theta: Double) = {
    val c = cos(theta)
    val s = sin(theta)
    Orientation(
      Position(c, s, 0), Position(-s, c, 0), Position(0, 0, 1))
  }
}


/**
 * A value class wrapper for approximate comparison of double
 * precision values.
 */
final class DoubleApprox(val x: Double) extends AnyVal {

  /**
   * Compare two double-precision values up to a fudge factor.
   *
   * @param that the double value to compare `this` to.
   * @param epsilon the fudge factor.
   */
  @inline
  def ~=(that: Double, epsilon: Double = DoubleApprox.epsilon): Boolean =
    DoubleApprox.compApprox(this.x, that)
}

/** Companion object of [[DoubleApprox]]. */
object DoubleApprox {

  /**
   * Compare two double-precision values up to a fudge factor.
   *
   * @param x the first operand.
   * @param y the second operand.
   * @param epsilon the fudge factor.
   */
  @inline def compApprox(x: Double, y: Double,
    epsilon: Double = DoubleApprox.epsilon): Boolean = abs(x - y) <= epsilon

  /** Fudge factor for determinant comparison. */
  @inline final def epsilon = 1E-8

  /** A view from `Double` to [[DoubleApprox]]. */
  implicit def doubleToApprox(x: Double): DoubleApprox = new DoubleApprox(x)
}
