package scala

import scala.language.implicitConversions

import scala.collection.mutable

trait Marks
{
  type T <: Markable

  abstract class MarkType {
    def toInt: Int
  }
  final case object Modified extends MarkType {
    @inline def toInt = 1
  }
  final case object SideEffect extends MarkType {
    @inline def toInt = 2
  }
  final case object Visited extends MarkType {
    @inline def toInt = 3
  }
  // etc...

  implicit def markTypeToInt(mt: MarkType): Int = mt.toInt

  trait Markable extends Enumeration
  {
    /**
     * Marker flag for agents to be considered checked (used to
     * track clashes in actions, updates made by actions, etc.).
     */
    protected[Marks] val _marked: mutable.BitSet = mutable.BitSet()

    /**
     * Marker flag for agents to be considered checked (used to
     * track clashes in actions, updates made by actions, etc.).
     */
    @inline def marked: mutable.BitSet = _marked

  }

  /** The collection used to track marked agents. */
  private val _markedAgents: mutable.HashMap[MarkType, List[T]] = mutable.HashMap()

  /**
   * The collection used to track marked agents.
   *
   * NOTE: A call to this method will prune the marked agents list
   * before returning it, which may take O(n) time.  However, if the
   * list is subsequently iterated over, the total amortized access
   * time of the call to this method is just O(1) as the pruning
   * time is proportional to previous calls to [[Mixture.unmark]]
   * plus the number of subsequent iteration steps over the pruned
   * list.
   *
   * FIXME: We could do better.  We could prune the list lazily,
   * thereby fixing the amortized cost to O(1) even for cases where
   * the list is not iterated over completely.
   *
   * @return the collection used to track marked agents.
   */
  def markedAgents(mt: MarkType) = {
    _markedAgents(mt) = _markedAgents(mt) filter (_.marked contains mt)
    _markedAgents(mt)
  }

  /**
   * Mark a given agent and add it to the marked agents list (unless
   * it was already marked).
   */
  @inline def mark(x: T, mt: MarkType) {
    if (! (x.marked contains mt)) {
      x._marked += mt
      _markedAgents(mt) = x :: _markedAgents(mt)
    }
  }

  /** Unmark a given agent. */
  @inline def unmark(x: T, mt: MarkType) {
    x._marked -= mt
  }

  /**
   * Unmark all the agents in the marked agents list and clear the
   * list.
   */
  @inline def clearMarkedAgents {
    for (agent <- _markedAgents.values.flatten)
      agent._marked.clear()
    _markedAgents.clear()
  }
}

