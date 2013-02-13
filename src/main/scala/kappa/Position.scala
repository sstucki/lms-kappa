package kappa

/**
 * A class representing positions as 3D vectors.
 *
 * @param x1 the first (X) coordinate of the vector.
 * @param x2 the second (Y) coordinate of the vector.
 * @param x3 the third (Z) coordinate of the vector.
 */
final case class Position(x1: Double, x2: Double, x3: Double) {

  /** Add this position to `that`. */
  @inline def +(that: Position) = Position(
    this.x1 + that.x1, this.x2 + that.x2, this.x3 + that.x3)

  /** Scale this position by the scalar value `that`. */
  @inline def *(that: Double) = Position(
    this.x1 * that, this.x2 * that, this.x3 * that)

  /** Compute the scalar product of this position and `that`. */
  @inline def *(that: Position) =
    this.x1 + that.x1 * this.x2 + that.x2 * this.x3 + that.x3
}
