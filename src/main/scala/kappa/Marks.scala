package kappa

import scala.language.implicitConversions

import scala.collection.mutable


// RHZ: This trait should be inside Marks to have val _marked protected[Marks]
// The problem then is that we would need to make Agent an inner class of Mixture
// Are we prepared for that?
trait Markable
{
  /**
   * Marker flag for agents to be considered checked (used to
   * track clashes in actions, updates made by actions, etc.).
   */
  /*protected[Marks]*/ val _marked: mutable.BitSet = mutable.BitSet()

  /**
   * Marker flag for agents to be considered checked (used to
   * track clashes in actions, updates made by actions, etc.).
   */
  @inline def hasMark(mt: MarkType): Boolean = _marked contains mt.toInt
  // The implicit conversion is not in scope so we have to call .toInt
}


// Mark types
abstract class MarkType {
  def toInt: Int
}
final object Updated extends MarkType {
  @inline def toInt = 1
}
final object SideEffect extends MarkType {
  @inline def toInt = 2
}
final object Visited extends MarkType {
  @inline def toInt = 3
}
// etc...


trait Marks
{
  type T <: Markable

  implicit def markTypeToInt(mt: MarkType): Int = mt.toInt

  /** Number of different mark types we can initially have for agents.
   *  NOTE: We use 64 because BitSet is initialised to 64 bits.
   */
  private var _markedAgentsLength: Int = 64

  /** The collection used to track marked agents. */
  private var _markedAgents: Array[List[T]] = Array.fill(_markedAgentsLength)(List())

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
    _markedAgents(mt) = _markedAgents(mt) filter (_ hasMark mt)
    _markedAgents(mt)
  }

  /**
   * Mark a given agent and add it to the marked agents list (unless
   * it was already marked).
   */
  @inline def mark(x: T, mt: MarkType) {
    if (mt > _markedAgentsLength) {
      _markedAgents ++= Array.fill(_markedAgentsLength)(List())
      _markedAgentsLength *= 2
    }
    if (! (x hasMark mt)) {
      x._marked += mt
      _markedAgents(mt) = x :: _markedAgents(mt)
    }
  }

  /** Unmark a given agent. */
  @inline def unmark(x: T, mt: MarkType) {
    x._marked -= mt
  }

  @inline def unmark(x: T) {
    x._marked.clear
  }

  /**
   * Unmark all the agents in the marked agents list and clear the
   * list.
   */
  @inline def clearMarkedAgents(mt: MarkType) {
    for (agent <- _markedAgents(mt))
      agent._marked.clear
    _markedAgents(mt) = List()
  }
}

