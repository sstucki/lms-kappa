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

  /**
   * Returns a the least upper bound of this matchable and `that`, if
   * it exists.
   *
   * Values returned by this method should be ''least upper bounds''
   * of the type `T` w.r.t. the partial order defined by the `matches`
   * method.  An instance `val z = (x join y)` of `T` is the least
   * upper bound (or ''join'', or ''supremum'') of two matchables `x`
   * and `y` (of type `T`) if it satisfies all of the following:
   *
   *  - `z matches x` and `z matches y` (`z` is an upper bound of `x`
   *    and `y`),
   *
   *  - for any instance `w` of type `T`, such that `w matches x` and
   *    `w matches y`, we have `w matches z` (`z` is greater than or
   *    equal to any upper bound of `x` and `y`).
   *
   * Note that a LUB does not necessarily exist for any pair of
   * matchables.  If every pair of matchables with underlying type `T`
   * have a LUB then the instances of `T` form a ''join-semilattice''
   * (or ''upper semilattice'').
   *
   * See also [[http://en.wikipedia.org/wiki/Join_and_meet]].
   *
   * @return `Some(z)`, where `z` is the least upper bounds of `this`
   *         and `that`, or `None` if no such `z` exists.
   */
  def join(that: T): Option[T]

  /**
   * Returns a the greatest lower bound of this matchable and `that`,
   * if it exists.
   *
   * Values returned by this method should be ''greatest lower
   * bounds'' of the type `T` w.r.t. the partial order defined by the
   * `matches` method.  An instance `val z = (x meet y)` of `T` is the
   * greatest lower bound (or ''meet'', or ''infimum'') of two
   * matchables `x` and `y` (of type `T`) if it satisfies all of the
   * following:
   *
   *  - `x matches z` and `y matches z` (`z` is a lower bound of `x`
   *    and `y`),
   *
   *  - for any instance `w` of type `T`, such that `x matches w` and
   *    `y matches w`, we have `z matches w` (`z` is lower than or
   *    equal to any lower bound of `x` and `y`).
   *
   * Note that a GLB does not necessarily exist for any pair of
   * matchables.  If every pair of matchables with underlying type `T`
   * have a GLB then the instances of `T` form a ''meet-semilattice''
   * (or ''lower semilattice'').
   *
   * See also [[http://en.wikipedia.org/wiki/Join_and_meet]].
   *
   * @return `Some(z)`, where `z` is the greatest lower bounds of
   *         `this` and `that`, or `None` if no such `z` exists.
   */
  def meet(that: T): Option[T]

  /**
   * Checks if this matchable can be used in mixtures.
   *
   * @return `true` if this matchable is valid in a mixture.
   */
  def isComplete: Boolean

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
   * This method defines a new binary "matches" relation `M(ox, oy)`
   * (w.r.t. the relation `f`):
   *
   *  - `M(None, oy)`, for any `oy`,
   *  - `M(Some(x), Some(y))`, if and only if `f(x, y)`.
   *
   * @tparam T the underlying type of the `Option[T]` values.
   * @param ox the `Option` value to match.
   * @param oy the `Option` value to match against.
   * @param f a function representing the "matches" relation.
   * @return `true` if `ox` matches `oy` according to `M(ox, oy)`.
   */
  @inline final def optionMatches[T](ox: Option[T], oy: Option[T])(
    f: (T, T) => Boolean): Boolean =
      (ox, oy) match {
        case (None, _) => true
        case (Some(_), None) => false
        case (Some(x), Some(y)) => f(x, y)
      }

  /**
   * Compute the join of two `Option[T]` values for given underlying
   * join operation `f` on `T`s.
   *
   * This method defines a new join operation `J(ox, oy)` (w.r.t. the
   * join operation `f` on `T`s):
   *
   *  - `J(ox, None) = Some(None)`, for any `ox`,
   *  - `J(None, oy) = Some(None)`, for any `oy`,
   *  - `J(Some(x), Some(y)) = Some(f(x, y))`.
   *
   * @tparam T the underlying type of the `Option[T]` values.
   * @param ox the first `Option[T]` value operand.
   * @param oy the second `Option[T]` value operand.
   * @param f the join operation over `T`.
   * @return the join of `ox` and `oy` as defined above.
   */
  @inline final def optionJoin[T](ox: Option[T], oy: Option[T])(
    f: (T, T) => Option[T]): Option[Option[T]] =
      (ox, oy) match {
        case (Some(x), Some(y)) => Some(f(x, y))
        case _ => Some(None)
      }

  /**
   * Compute the meet of two `Option[T]` values for given underlying
   * meet operation `f` on `T`s.
   *
   * This method defines a new meet operation `M(ox, oy)` (w.r.t. the
   * join operation `f` on `T`s):
   *
   *  - `M(ox, None) = Some(ox)`, for any `ox`,
   *  - `M(None, oy) = Some(oy)`, for any `oy`,
   *  - `M(Some(x), Some(y)) = Some(f(x, y))`, if `f(x, y)` exists,
   *    and `None` otherwise.
   *
   * @tparam T the underlying type of the `Option[T]` values.
   * @param ox the first `Option[T]` value operand.
   * @param oy the second `Option[T]` value operand.
   * @param f the meet operation over `T`.
   * @return the meet of `ox` and `oy` as defined above.
   */
  @inline final def optionMeet[T](ox: Option[T], oy: Option[T])(
    f: (T, T) => Option[T]): Option[Option[T]] =
      (ox, oy) match {
        case (_, None) => Some(ox)
        case (None, _) => Some(oy)
        case (Some(x), Some(y)) => f(x, y) map (Some(_))
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

    @inline def matches(that: OptionMatchable[T]) =
      optionMatches(this.om, that.om)(_ matches _)

    @inline override def isEquivTo[U <: OptionMatchable[T]](that: U): Boolean =
      (this.om, that.om) match {
        case (None, None) => true
        case (Some(m1), Some(m2)) => m1 isEquivTo m2
        case _ => false
      }

    @inline def join(that: OptionMatchable[T]) =
      optionJoin(this.om, that.om)(_ join _) map (new OptionMatchable(_))

    @inline def meet(that: OptionMatchable[T]) =
      optionMeet(this.om, that.om)(_ meet _) map (new OptionMatchable(_))

    @inline def isComplete = (this.om map (_.isComplete)) getOrElse false
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
