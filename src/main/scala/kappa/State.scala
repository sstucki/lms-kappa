package kappa

import scala.language.implicitConversions

import scala.math.PartialOrdering
//import scala.math.PartiallyOrdered


/**
 * A trait for states.
 *
 * States ar characterized by two boolean relations, each of which is
 * represented by an abstrac method of this trait:
 *
 *  1. [[matches]] is a binary relation that defines a partial order on
 *     states, i.e. it specifies whether `this` state "matches" `that`
 *     state;
 *
 *  2. [[isConcrete]] is an unary relation that defines whether a
 *     given state is admissible in a mixture, i.e whether it is
 *     a "concrete" state.
 *
 * Examples of states are agent, site and link states.
 *
 * Example usage:
 * {{{
 * case class MyState(name: String) extends State[MyState] {
 *   override def toString = name
 *   def matches(that: MyState) = this.name == that.name
 * }
 * }}}
 *
 * sstucki: It would seem natural for this trait to extend
 * `scala.math.PartiallyOrdered[State]` for compatibility with the
 * standard library.  However, `<=` needs to be efficient (because
 * it used in the "hot zone" of the simulator) and
 * [[scala.math.PartiallyOrdered]] seems to carry quite a bit of an
 * overhead (it seems to be covariant in its type parameter, which
 * requires implementations of its `tryCompare` method to be
 * "non-trivial").  Furthermore, I could not find a single use case of
 * [[scala.math.PartiallyOrdered]] anywhere from which to take
 * inspiration.
 */
trait State[A] {

  /**
   * Checks if this state can be used in mixtures.
   *
   * @return `true` if this sate is valid in a mixture.
   */
  def matches(that: A): Boolean

  /**
   * Checks if this state can be used in mixtures.
   *
   * @return `true` if this sate is valid in a mixture.
   */
  def isConcrete: Boolean

  // FIMXE: In the future we might want to provide these for compatibility?

  // /** Returns `true` if `this` is less than or equal to `that`. */
  // def <=(that: A): Boolean

  // /** Returns `true` if `this` is less than `that`. */
  // def <(that: State[A]) = (this <= that) && !(that <= this)

  // /** Returns `true` if `this` is greater than or equal to `that`. */
  // def >=(that: State[A]) = (that <= this)

  // /** Returns `true` if `this` is greater than `that`. */
  // def >[B <: State[A]](that: B) = (that <= this) && !(this <= that)

  // /**
  //  * Result of comparing `this` with `that`.
  //  *
  //  * Returns `x` where:
  //  *
  //  * - `x < 0` when `this < that`,
  //  *
  //  * - `x == 0` when `this == that`,
  //  *
  //  * - `x > 0` when `this > that`.
  //  */
  // def tryCompareTo(that: State[A]): Option[Int] =
}

object State {

  /** View from `State` to `PartialOrdering[State]` */
  implicit def stateToPartialOrdering[T <% State[T]]: PartialOrdering[T] =
    new PartialOrdering[T] {
      @inline def lteq(x: T, y: T): Boolean = y matches x
      @inline def tryCompare(x: T, y: T): Option[Int] =
        if (lteq(x, y)) {
          if (lteq(x, y)) Some(0) else Some(-1)
        } else {
          if (lteq(x, y)) Some(1) else None
        }
    }

  /**
   * Compare two Option values using a "matches" relation.
   *
   * This function defines a new binary "matches" relation `M(x, y)`
   * (w.r.t. the relation `mf`):
   *
   *  - `M(None, y)`, for any `y`
   *  - `M(Some(x), Some(y))`, if and only if `mf(x, y)`
   *
   * @tparam T the underlying type of the option values.
   * @param x the option value to match.
   * @param y the option value to match against.
   * @param mf a function representing the "matches" relation.
   * @return `true` if `x` matches `y` according to `mf`.
   */
  @inline def matchesOption[T](x: Option[T], y: Option[T])(
    mf: (T, T) => Boolean): Boolean =
      (x, y) match {
        case (None, _) => true
        case (_, None) => false
        case (Some(x), Some(y)) => mf(x, y)
      }
}
