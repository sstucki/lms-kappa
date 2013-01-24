package kappa

import scala.math.PartialOrdering
//import scala.math.PartiallyOrdered
import scala.language.implicitConversions


/**
 * A trait for objects that can be matched.
 *
 * Examples of matchables are patterns, agents, sites, links, as well
 * as states (e.g. agent, site and link states).
 *
 * Matchables ar characterized by two boolean relations, each of which
 * is represented by an abstract method of this trait:
 *
 *  1. [[matches]] is a binary relation that defines a partial order
 *     on matchables, i.e. it specifies whether `this` matchable
 *     "matches" `that` matchable;
 *
 *  2. [[isComplete]] is an unary realtion that defines whether a
 *     given matchable is admissible in a mixture, i.e whether it is a
 *     "concrete" instance of the given matchable class.
 *
 * Example usage:
 * {{{
 * case class MyMatchable(name: String) extends Matchable[MyMatchable] {
 *   override def toString = name
 *   def matches(that: MyMatchable) = this.name == that.name
 * }
 * }}}
 *
 * sstucki: It would seem natural for this trait to extend
 * `scala.math.PartiallyOrdered[Matchable]` for compatibility with the
 * standard library.  However, `<=` needs to be efficient (because it
 * used in the "hot zone" of the simulator) and
 * [[scala.math.PartiallyOrdered]] seems to carry quite a bit of an
 * overhead (it seems to be covariant in its type parameter, which
 * requires implementations of its `tryCompare` method to be
 * "non-trivial").  Furthermore, I could not find a single use case of
 * [[scala.math.PartiallyOrdered]] anywhere from which to take
 * inspiration.
 */
trait Matchable[T] extends Any {

  this: T =>

  /**
   * Checks if this matchable matches `that`.
   *
   * @return `true` if this matchable matches `that`.
   */
  def matches(that: T): Boolean

  /**
   * Checks if this matchable can be used in mixtures.
   *
   * @return `true` if this matchable is valid in a mixture.
   */
  def isComplete: Boolean

  /**
   * Check if this matchable is equivalent to `that`.
   *
   * The function defines the following equivalence relation:
   *
   *  - `isEquivTo(x, y)` iff `(x matches y) && (y matches x)`
   *
   * @return `true` if and only if `this` and `that` are equivalent.
   */
  @inline
  def isEquivTo[U <: T with Matchable[T]](that: U): Boolean =
    (this matches that) && (that matches this)

  // FIMXE: In the future we might to provide these for compatibility?

  // /** Returns `true` if `this` is less than or equal to `that`. */
  // def <=(that: T): Boolean

  // /** Returns `true` if `this` is less than `that`. */
  // def <(that: Matchable[T]) = (this <= that) && !(that <= this)

  // /** Returns `true` if `this` is greater than or equal to `that`. */
  // def >=(that: Matchable[T]) = (that <= this)

  // /** Returns `true` if `this` is greater than `that`. */
  // def >[B <: Matchable[T]](that: B) = (that <= this) && !(this <= that)

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
  // def tryCompareTo(that: Matchable[T]): Option[Int]
}

object Matchable {

  /**
   * Compare two `Option` values using a "matches" relation.
   *
   * This function defines a new binary "matches" relation `M(ox, oy)`
   * (w.r.t. the relation `mf`):
   *
   *  - `M(None, oy)`, for any `oy`
   *  - `M(Some(x), Some(y))`, if and only if `mf(x, y)`
   *
   * @tparam T the underlying type of the `Option[T]` values.
   * @param ox the `Option` value to match.
   * @param oy the `Option` value to match against.
   * @param mf a function representing the "matches" relation.
   * @return `true` if `ox` matches `oy` according to `M(ox, oy)`.
   */
  @inline final def optionMatches[T](ox: Option[T], oy: Option[T])(
    mf: (T, T) => Boolean): Boolean =
      (ox, oy) match {
        case (None, _) => true
        case (Some(_), None) => false
        case (Some(x), Some(y)) => mf(x, y)
      }

  /**
   * A wrapper class to match `Option[Matchable[T]]` instances.
   *
   * This class extends the binary matches relation over a matchable
   * type `T` to options values of `T`.  The relation is defined as in
   * [[Matchable.optionMatches]] with the [[Matchable.matches]] method
   * of the corresponding `Matchable[T]` type providing the inner
   * "matches" relation.
   *
   * @tparam T the underlying type of the `Option[Matchable[T]]` values.
   * @param ox the `Option[Matchable[T]]` value to match.
   * @param oy the `Option[Matchable[T]]` value to match against.
   * @return `true` if `ox` matches `oy` according to `M(ox, oy)`.
   */
  final class OptionMatchable[T <: Matchable[T]](val om: Option[T])
      extends AnyVal with Matchable[OptionMatchable[T]] {
    def matches(that: OptionMatchable[T]) =
      optionMatches(this.om, that.om)(_ matches _)
    def isComplete = (this.om map (_.isComplete)) getOrElse false
  }

  /** View from `Option[Matchable[T]]` to `OptionMatchable[T]` */
  implicit def matchableOptionAsOptionMatchable[T <: Matchable[T]](om: Option[T])
      : OptionMatchable[T] = new OptionMatchable(om)

  /** View from `Matchable` to `PartialOrdering[Matchable]` */
  implicit def matchableAsPartialOrdering[T <% Matchable[T]]
      : PartialOrdering[T] =
    new PartialOrdering[T] {
      @inline def lteq(x: T, y: T): Boolean = y matches x
      @inline def tryCompare(x: T, y: T): Option[Int] =
        if (lteq(x, y)) {
          if (lteq(x, y)) Some(0) else Some(-1)
        } else {
          if (lteq(x, y)) Some(1) else None
        }
    }

  // @inline def sequals[T <: Matchable[T]](ls: T, rs: T): Boolean =
  //   (ls matches rs) && (rs matches ls)
}
