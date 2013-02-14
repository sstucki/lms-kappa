package kappa

/**
 * A class representing orientations in 3D euclidean space.
 *
 * NOTE: The orientation is represented as a set of 3 the basis
 * vectors, i.e. the column vectors of the matrix.
 *
 * @param e1 the first (X) basis vector of the orientation matrix.
 * @param e2 the second (Y) basis vector of the orientation matrix.
 * @param e3 the third (Z) basis vector of the orientation matrix.
 */
final class Orientation private (
  val e1: Position, val e2: Position, val e3: Position) {

  /** Apply this rotation to a position `that`. */
  @inline def *(that: Position): Position =
    this.e1 * that.x1 + this.e2 * that.x2 + this.e3 * that.x3

  /** Combine (multiply) this rotation with another one. */
  @inline def *(that: Orientation): Orientation = Orientation(
    this * that.e1, this * that.e2, this * that.e2)

  /** Return the inverse this orientation matrix. */
  @inline def inv = t

  /** Return the transpose this orientation matrix. */
  @inline def t = Orientation(
    Position(e1.x1, e2.x1, e3.x1),
    Position(e1.x2, e2.x2, e3.x2),
    Position(e1.x3, e2.x3, e3.x3))

  /** Return the determinant of this orientation matrix. */
  @inline def det = 1

  /** Return the determinant of this orientation matrix. */
  private def actualDet =
    e1.x1 * (e2.x2 * e3.x3 - e3.x2 * e2.x3) +
    e1.x2 * (e3.x1 * e2.x3 - e2.x1 * e3.x3) +
    e1.x3 * (e2.x1 * e3.x2 - e3.x1 * e2.x2)

  /** Check if this matrix is indeed in SO(3). */
  def isConsistent: Boolean = actualDet == 1
}

/** Companion object of the [[Orientation]] class. */
object Orientation {

  /**
   * Factory method for constructing [[Orientation]]s from 3
   * orthonormal basis vectors.
   *
   * The 3 basis vectors correspond to the column vectors of the
   * orientation matrix.
   *
   * @param e1 the first (X) basis vector of the orientation matrix.
   * @param e2 the second (Y) basis vector of the orientation matrix.
   * @param e3 the third (Z) basis vector of the orientation matrix.
   */
  def apply(e1: Position, e2: Position, e3: Position): Orientation = {
    val o = new Orientation(e1, e2, e2)
    if (o.isConsistent) o else throw new IllegalArgumentException(
      "attempt to create inconsistent orientation matrix")
  }

  /**
   * Factory method for constructing [[Orientation]]s from a 3x3
   * orientation matrix given as 9 individual values in row-major
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
   * orientation matrix given as a sequence of 9 individual values in
   * row-major order.
   *
   * @param m a sequence representing a 3x3 orientation matrix in
   *        row-major order.
   */
  def apply(m: Seq[Double]): Orientation = {
    if (m.length != 9) throw new IllegalArgumentException(
      "attempt to create 3x3 orientation matrix from sequence of " +
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
    val c = scala.math.cos(theta)
    val d = 1 - c
    val s = scala.math.sin(theta)
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

  /**
   * Factory method for constructing [[Orientation]]s around the
   * X-axis from a given an angle.
   *
   * @param theta the rotation angle around the X-axis in radians.
   */
  def x(theta: Double) = {
    val c = scala.math.cos(theta)
    val s = scala.math.sin(theta)
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
    val c = scala.math.cos(theta)
    val s = scala.math.sin(theta)
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
    val c = scala.math.cos(theta)
    val s = scala.math.sin(theta)
    Orientation(
      Position(c, s, 0), Position(-s, c, 0), Position(0, 0, 1))
  }
}
