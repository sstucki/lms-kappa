package kappa

/**
 * A class representing positions as 3D vectors.
 *
 * @param x1 the first (X) coordinate of the vector.
 * @param x2 the second (Y) coordinate of the vector.
 * @param x3 the third (Z) coordinate of the vector.
 */
final case class Position(x1: Double, x2: Double, x3: Double) {

  import DoubleApprox._

  /** Add this position to `that`. */
  @inline def +(that: Position) = Position(
    this.x1 + that.x1, this.x2 + that.x2, this.x3 + that.x3)

  /** Subtract `that` from this position. */
  @inline def -(that: Position) = Position(
    this.x1 - that.x1, this.x2 - that.x2, this.x3 - that.x3)

  /** Scale this position by the scalar value `that`. */
  @inline def *(that: Double) = Position(
    this.x1 * that, this.x2 * that, this.x3 * that)

  /** Compute the scalar product of this position and `that`. */
  @inline def *(that: Position) =
    this.x1 * that.x1 + this.x2 * that.x2 + this.x3 * that.x3

  /** Compute the norm (or length, or modulus) of this position. */
  @inline def norm = scala.math.sqrt(this * this)

  /**
   * Compare this position (element-wise) to `that` up to a fudge
   * factor.
   *
   * @param that the position to compare `this` to.
   * @param epsilon the fudge factor.
   */
  def ~=(that: Position, epsilon: Double = DoubleApprox.epsilon): Boolean =
    (this.x1 ~= (that.x1, epsilon)) &&
    (this.x2 ~= (that.x2, epsilon)) &&
    (this.x3 ~= (that.x3, epsilon))

  @inline override def toString = "[" + x1 + ", " + x2 + ", " + x3 + "]"
}

object Position {
  /** Get a position vector from spherical coordinates.
   *
   * @param theta the zenith angle, ie the angle in the x-y plane.
   * @param phi the azimuth angle, ie the angle in the plane created
   *            by the z axis and orthogonal projection of the zenith.
   * @param radius the euclidean distance between the tip of the vector
   *               and the origin.
   */
  def fromSpherical(radius: Double, theta: Double, phi: Double) = {
    import scala.math._
    Position(radius * sin(theta) * cos(phi),
             radius * sin(theta) * sin(phi),
             radius * cos(theta))
  }
}

