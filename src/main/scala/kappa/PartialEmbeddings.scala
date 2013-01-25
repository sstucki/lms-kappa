package kappa

import scala.collection.mutable

trait PartialEmbeddings {
  this: Mixtures with Patterns =>

  /**
   * A class reperesenting an embedding form a single connected
   * component of a site graph into a mixture.
   *
   * ''WARNING'': For convenience, this class provides the interface
   * of a `Seq[Mixture.Agent]`.  However, using some methods from the
   * `Seq` API might not result in the expected behavior.  E.g. `++`
   * will return a `Seq[PartialEmbedding]` rather than the expected
   * `PartialEmbedding`.
   *
   * @param inj an array of [[Mixtures#Mixture.Agent]]s representing
   *        the injection from pattern agent indices to mixture
   *        agents.
   * @param component the connected [[Patterns#Pattern.Component]]
   *        (of `pattern`) that conconstitutes the domain of this
   *        embedding.
   */
  final case class PartialEmbedding private (
    val inj: Array[Mixture.Agent], val component: Pattern.Component)
      extends Seq[Mixture.Agent] {

    type Source = AgentIndex
    type Target = Mixture.Agent

    // TODO: Not used. Remove.
    // /**
    //  * The index of the partial embedding within the collection of
    //  * embeddings of the connected component that constitutes the
    //  * domain of this embedding.
    //  */
    // var index: EmbeddingIndex = -1

    /**
     * Returns this [[PartialEmbedding]] as a map from agent indices
     * to [[Mixtures#Mixture.Agent]]s.
     *
     * @return this [[PartialEmbedding]] as a map from agent indices
     * to [[Mixtures#Mixture.Agent]]s.
     */
    def toMap: Map[Source, Target] = mapIterator.toMap

    // -- Convenience functions from Map[Source, Target] API --

    /**
     * Selects the first pair of this partial embedding.
     *
     * @return the first pair of this partial embedding.
     */
    @inline def mapHead: (Source, Target) = (0, inj.head)

    @inline def get(key: Source): Option[Target] =
      if (key > inj.size) None else Some(inj(key))

    @inline def mapIterator: Iterator[(Source, Target)] =
      for ((v, k) <- inj.iterator.zipWithIndex) yield (k, v)

    // TODO: Potentially confusing?
    @inline def +(p: (Source, Target)) = this updated (p._1, p._2)


    // -- Core Seq[Target] API --

    @inline def apply(idx: Source): Target = inj(idx)

    @inline def iterator: Iterator[Target] = inj.iterator

    @inline def length: Int = inj.length


    // -- Extra Seq[Target] API --

    @inline def :+(elem: Target) = this.copy(inj = this.inj :+ elem)

    @inline override def foreach[U](f: Target => U): Unit =
      inj foreach f

    @inline def updated(index: Source, elem: Target) =
      this.copy(inj = this.inj updated (index, elem))

    // -- Equals API --
    override def canEqual(that: Any) = that.isInstanceOf[PartialEmbedding]

    // -- Any API --
    /**
     * Compares the this embedding with the argument (`that`) for
     * equality.
     *
     * The only reason for overloading this method is efficiency.  We
     * know that a pair `(u, v)` of agents from a pattern component
     * and mixture can extend to at most one total embedding between
     * from the pattern component to the mixture.  Hence it's
     * sufficient to test the pattern components, mixtures and head
     * pairs of the two embeddings for equality.
     *
     * @return `true` if the argument is a reference to the receiver
     *         agent; `false` otherwise.
     */
    override def equals(that: Any): Boolean = that match {
      case that: PartialEmbeddings#PartialEmbedding =>
        (that canEqual this) &&
        (this.component == that.component) &&
        (this.head == that.head)
      case _ => false
    }

    /**
     * Calculate a hash code value for this partial embedding.
     *
     * See [[PartialEmbedding.equals]] for why this method is
     * overridden.
     *
     * @return the hash code value for this partial embedding.
     */
    override def hashCode(): Int =
      this.component.## + this.head.##

    override def toString =
      "PE(" + (if (component == null) "?" else component.index) +
        ": " + toMap.mkString(", ") + ")"
  }


  object PartialEmbedding {

    /**
     * Extend a pattern/mixture agent pair into a partial embedding.
     *
     * @param u the pattern agent in the agent pair
     * @param v the mixture agent in the agent pair
     * @returns `Some(pe)`, where `pe` is a (partial) embedding from
     *          the pattern containing `u` into the mixture containing
     *          `v`, or `None` if no such embedding exists.
     */
    def apply(u: Pattern.Agent, v: Mixture.Agent): Option[PartialEmbedding] = {
      val component = u.component
      val inj = new Array[Mixture.Agent](component.length)
      if (extendInjection(u, v, inj, null))
        Some(new PartialEmbedding(inj, u.component))
      else None
    }

    /**
     * Extend a multiple pattern/mixture agent pairs into a set of
     * partial embeddings.
     *
     * All pattern/mixture agent pairs in `ps` must belong to the same
     * pattern and mixture, respectively.  The partial embeddings
     * returned by the method are guaranteed to be unique w.r.t each
     * other.
     *
     * @param ps a collection of pattern/mixture agent pairs from the
     *        same pattern and mixture, respectively.
     * @returns all the (partial) embeddings the containing any of the
     *          pairs in `ps`.
     */
    def apply(ps: Iterable[(Pattern.Agent, Mixture.Agent)])
        : Iterable[PartialEmbedding] = {

      if (ps.isEmpty) Iterable.empty
      else {
        val component = ps.head._1.component
        val conflicts =
          new Array[mutable.HashSet[Mixture.Agent]](component.length)
        for (i <- 0 until conflicts.size) {
          conflicts(i) = new mutable.HashSet()
        }
        (for ((u, v) <- ps) yield {
          val inj = new Array[Mixture.Agent](component.length)
          if (extendInjection(u, v, inj, conflicts))
            Some(new PartialEmbedding(inj, u.component))
          else None
        }).flatten
      }
    }

    /**
     * Extend a pair of pattern agents into a partial embedding.
     *
     * @param u the first pattern agent in the agent pair
     * @param v the second pattern agent in the agent pair
     * @returns `Some(pe)`, where `pe` is a (partial) embedding from
     *          the pattern containing `u` into the pattern containing
     *          `v`, or `None` if no such embedding exists.
     */
    def findEmbedding(u: Pattern.Agent, v: Pattern.Agent)
        : Option[Array[Pattern.Agent]] = {

      val component = u.component
      val inj = new Array[Pattern.Agent](component.length)
      if (extendInjection(u, v, inj, null)) Some(inj)
      else None
    }

    /**
     * Extend a partial injection from agent indices to agents
     * represented as an array of [[Pattern.Agent]]s through a
     * traversal of the respective site graphs.
     *
     * @param u the next agent in the pre-image of the injection to
     *        inspect in the traversal.
     * @param v the next agent in the image of the injection to
     *        inspect in the traversal.
     * @param inj the partial injection to extend.
     * @param conflicts a conflict map used to avoid the construction
     *        of redundant injections by recording injection pairs
     *        encountered during previous traversals (only used
     *        during multiple traversals).
     * @returns `true` if the injection in `inj` is total.
     */
    private def extendInjection(
      u: Pattern.Agent, v: Pattern.Agent, inj: Array[Pattern.Agent],
      conflicts: Array[mutable.HashSet[Pattern.Agent]]): Boolean = {
      val i = u.index
      if (inj(i) != null) inj(i) == v
      else if (conflicts != null && (conflicts(i) contains v)) false
      else {
        if (u matches v) {
          inj(i) = v
          if (conflicts != null) conflicts(i) += v
          (0 until u.sites.size) forall { j =>
            (u.neighbour(j), v.neighbour(j)) match {
              case (None, _) => true
              case (Some((w1, _)), Some((w2, _))) => {
                extendInjection(w1, w2, inj, conflicts)
              }
              case _ => false
            }
          }
        } else false
      }
    }

    /**
     * Extend a partial injection from agent indices to agents
     * represented as an array of [[Mixture.Agent]]s through a
     * traversal of the respective site graphs.
     *
     * @param u the next agent in the pre-image of the injection to
     *        inspect in the traversal.
     * @param v the next agent in the image of the injection to
     *        inspect in the traversal.
     * @param inj the partial injection to extend.
     * @param conflicts a conflict map used to avoid the construction
     *        of redundant injections by recording injection pairs
     *        encountered during previous traversals (only used
     *        during multiple traversals).
     * @returns `true` if the injection in `inj` is total after
     *          expansion.
     */
    // FIXME: Code duplication.  We could really profit from a
    // common interface for Mixture.{Agent,Site,Link} and
    // Site.{Agent,Site,Link}.
    private def extendInjection(
      u: Pattern.Agent, v: Mixture.Agent, inj: Array[Mixture.Agent],
      conflicts: Array[mutable.HashSet[Mixture.Agent]]): Boolean = {
      val i = u.index
      if (inj(i) != null) inj(i) == v
      else if (conflicts != null && (conflicts(i) contains v)) false
      else {
        if (u matches v) {
          inj(i) = v
          if (conflicts != null) conflicts(i) += v
          (0 until u.sites.size) forall { j =>
            (u.neighbour(j), v.neighbour(j)) match {
              case (None, _) => true
              case (Some((w1, _)), Some((w2, _))) =>
                extendInjection(w1, w2, inj, conflicts)
              case _ => false
            }
          }
        } else false
      }
    }
  }
}
