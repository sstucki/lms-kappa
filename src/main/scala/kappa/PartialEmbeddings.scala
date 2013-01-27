package kappa

import scala.collection.mutable

trait PartialEmbeddings {
  this: Patterns =>

  /**
   * A class representing a partial embedding between a pair of
   * connected pattern components.
   *
   * ''WARNING'': For convenience, this class provides the interface
   * of a `Seq[(Pattern.Agent, Pattern.Agent)]`.  However, using some
   * methods from the `Seq` API might not result in the expected
   * behavior.  E.g. `++` will return a `Seq[(Pattern.Agent,
   * Pattern.Agent)]` rather than the expected `PartialEmbedding`.
   *
   * FIXME:
   *
   *  1. Partial embeddings are spans.  As such they should have a
   *     "top" object, i.e. the [[Patterns#Pattern.Component]] that
   *     conconstitutes the domain of left and right legs (`leftInj`
   *     and `rightInj`).
   *
   *  2. We should probably have a class for embeddings between
   *     [[Patterns#Pattern.Component]]s and use those to represent
   *     left/right legs of this partial embedding.
   *
   * @param leftInj an array of [[Patterns#Pattern.Agent]]s representing
   *        the left leg of the span associated with this partial
   *        embedding, i.e. the (total) injection from the index set
   *        of `top` to the domain of this partial embedding.
   * @param rightInj an array of [[Patterns#Pattern.Agent]]s representing
   *        the right leg of the span associated with this partial
   *        embedding, i.e. the (total) injection from the index set
   *        of `top` to the codomain of this partial embedding.
   */
  final case class PartialEmbedding private (
    val leftInj: Array[Pattern.Agent], val rightInj: Array[Pattern.Agent])
      extends Seq[(Pattern.Agent, Pattern.Agent)] {

    type Source = AgentIndex
    type Target = Pattern.Agent

    /**
     * Returns this [[PartialEmbedding]] as a map from agent indices
     * to [[Patterns#Pattern.Agent]]s.
     *
     * @return this [[PartialEmbedding]] as a map from agent indices
     * to [[Patterns#Pattern.Agent]]s.
     */
    def toMap: Map[Source, Target] = mapIterator.toMap

    /**
     * Returns the inverse of this [[PartialEmbedding]].
     *
     * @return the inverse of this [[PartialEmbedding]].
     */
    def inverse = new PartialEmbedding(rightInj, leftInj)

    // -- Convenience functions from Map[Source, Target] API --

    /**
     * Selects the first pair of this partial embedding.
     *
     * @return the first pair of this partial embedding.
     */
    @inline def mapHead: (Source, Target) = (leftInj(0).index, rightInj(0))

    @inline def mapIterator: Iterator[(Source, Target)] =
      (0 until leftInj.size).iterator map {
        i => (leftInj(i).index, rightInj(i))
      }


    // -- Core Seq[(Target, Target)] API --

    @inline def apply(idx: Source): (Target, Target) =
      (leftInj(idx), rightInj(idx))

    @inline def iterator: Iterator[(Target, Target)] =
      (0 until leftInj.size).iterator map { i => (leftInj(i), rightInj(i)) }

    @inline def length: Int = leftInj.length


    // -- Extra Seq[(Target, Target)] API --

    @inline override def foreach[U](f: ((Target, Target)) => U): Unit =
      this.iterator foreach f

    // -- Any API --
    override def toString = {
      val lc = leftInj.head.component
      val rc = leftInj.head.component
      "PE(" + (if (lc == null) "?" else lc.index) +
      "/" + (if (rc == null) "?" else rc.index) +
      ": " + mapIterator.toMap.mkString(", ")+ ")"
    }
  }


  object PartialEmbedding {

    import Pattern._

    def apply(leftToRight: Map[Agent, Agent]): PartialEmbedding = {
      val (left, right) = leftToRight.toIterable.unzip
      new PartialEmbedding(left.toArray, right.toArray)
    }

    /**
     * Return all partial embedding components between two pattern
     * components.
     *
     * FIXME: "partial embedding component" doesn't mean anything.
     * Find a better term and explain!
     *
     * @param c1: the first component in the pair.
     * @param c2: the second component in the pair.
     * @return all partial embedding components between two pattern
     *         components.
     */
    def findPartialEmbeddings(c1: Component, c2: Component)
        : Iterable[PartialEmbedding] = {

      val conflicts =
        new Array[mutable.HashSet[Agent]](c1.length)
      for (i <- 0 until conflicts.size) {
        conflicts(i) = new mutable.HashSet()
      }

      (for (u <- c1; v <- c2) yield {
        val inj = new Array[Agent](c1.length)
        val meets = new Array[Agent](c1.length)
        if (extendPartialInjection(u, v, inj, conflicts, meets)) {
          val pairs =
            for (i <- (0 until c1.length) if inj(i) != null)
            yield (c1(i), inj(i))

          val (left, right) = pairs.unzip
          Some(new PartialEmbedding(left.toArray, right.toArray))
        } else None
      }).flatten
    }

    /**
     * Extend a partial injection between the agent sets of a pair
     * of pattern components through a traversal of the components.
     *
     * The partial injection is represented as an array of
     * [[Pattern.Agent]]s.  While extending the partial injection, the
     * method keeps track of previously encountered pairs of agents in
     * a conflict map, in order to avoid the construction of duplicate
     * injections.  The meet of every pair of agents encountered
     * during the traversal is stored to facilitate the construction
     * of glueings from the resulting partial injection.
     *
     * @param u the next agent in the pre-image of the map to inspect
     *        in the traversal.
     * @param v the next agent in the image of the map to inspect in
     *        the traversal.
     * @param inj the partial injection to extend.
     * @param conflicts a conflict map used to avoid the construction
     *        of redundant maps by recording pairs encountered during
     *        previous traversals.
     * @param meets the lists of meets of the traversed pairs of
     *        agents.
     * @returns `true` if no conflict occurred during expansion.
     */
    private def extendPartialInjection(
      u: Agent, v: Agent, inj: Array[Agent],
      conflicts: Array[mutable.HashSet[Agent]],
      meets: Array[Agent]): Boolean = {

      val i = u.index
      if (inj(i) != null) true
      else if (conflicts(i) contains v) false
      else {
        val m = u meet v
        if (m.isEmpty) true
        else {
          meets(i) = m.get
          inj(i) = v
          conflicts(i) += v
          (0 until u.sites.size) forall { j =>
            (u.neighbour(j), v.neighbour(j)) match {
              case (Some((w1, _)), Some((w2, _))) =>
                extendPartialInjection(w1, w2, inj, conflicts, meets)
              case _ => true
            }
          }
        }
      }
    }
  }
}

