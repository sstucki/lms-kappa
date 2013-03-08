package kappa

import scala.collection.mutable

trait ComponentEmbeddings {
  this: Agents with Mixtures with Patterns =>

  /**
   * A class representing an embedding from a single connected
   * component of a site graph into a mixture.
   *
   * ''WARNING'': For convenience, this class provides the interface
   * of a `Seq[Agents#Agent]`.  However, using some methods from the
   * `Seq` API might not result in the expected behavior.  E.g. `++`
   * will return a `Seq[Agents#Agent]` rather than the expected
   * `ComponentEmbedding`.
   *
   * @param T the target agent type.
   * @param inj an array of [[Agents#.Agent]]s representing
   *        the injection from pattern agent indices to mixture
   *        agents.
   * @param component the connected [[Patterns#Pattern.Component]]
   *        (of `pattern`) that conconstitutes the domain of this
   *        embedding.
   */
  final case class ComponentEmbedding[T <: Agent] private (
    val inj: Array[T], val component: Pattern.Component) extends Seq[T] {

    type Source = AgentIndex
    type Target = T

    // TODO: Not used. Remove.
    // /**
    //  * The index of the component embedding within the collection of
    //  * embeddings of the connected component that constitutes the
    //  * domain of this embedding.
    //  */
    // var index: EmbeddingIndex = -1

    /**
     * Returns this [[ComponentEmbedding]] as a map from agent indices
     * to agents.
     *
     * @return this [[ComponentEmbedding]] as a map from agent indices
     * to agents.
     */
    def toMap: Map[Source, Target] = mapIterator.toMap

    // -- Convenience functions from Map[Source, Target] API --

    /**
     * Selects the first pair of this component embedding.
     *
     * @return the first pair of this component embedding.
     */
    @inline def mapHead: (Source, Target) = (0, inj.head)

    @inline def get(key: Source): Option[Target] =
      if (key > inj.size) None else Some(inj(key))

    @inline def mapIterator: Iterator[(Source, Target)] =
      inj.iterator.zipWithIndex map (_.swap)


    // -- Core Seq[Target] API --

    @inline def apply(idx: Source): Target = inj(idx)

    @inline def iterator: Iterator[Target] = inj.iterator

    @inline def length: Int = inj.length


    // -- Extra Seq[Target] API --

    @inline override def foreach[U](f: Target => U): Unit =
      inj foreach f


    // -- Equals API --

    override def canEqual(that: Any) =
      that.isInstanceOf[ComponentEmbedding[_]]


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
      case that: ComponentEmbeddings#ComponentEmbedding[_] =>
        (that canEqual this) &&
        (this.component == that.component) &&
        (this.head == that.head)
      case _ => false
    }

    /**
     * Calculate a hash code value for this component embedding.
     *
     * See [[ComponentEmbedding.equals]] for why this method is
     * overridden.
     *
     * @return the hash code value for this component embedding.
     */
    override def hashCode(): Int =
      this.component.## + this.head.##

    override def toString =
      "CE(" + (if (component == null) "?" else component.index) +
        ": " + toMap.mkString(", ") + ")"
  }


  object ComponentEmbedding {

    /**
     * Extend a pattern/mixture agent pair into a component embedding.
     *
     * @param u the pattern agent in the agent pair
     * @param v the mixture agent in the agent pair
     * @returns `Some(ce)`, where `ce` is an embedding from
     *          the pattern component containing `u` into the mixture
     *          containing `v`, or `None` if no such embedding exists.
     */
    def findEmbedding(u: Pattern.Agent, v: Mixture.Agent)
        : Option[ComponentEmbedding[Mixture.Agent]] = {
      val component = u.component
      val inj = new Array[Mixture.Agent](component.length)
      v.mixture.clearMarkedAgents(Visited)
      if (extendInjection(u, v, inj)) {
        val ce = new ComponentEmbedding[Mixture.Agent](inj, u.component)

        // Register the new embedding in the lift sets of all its
        // target agents.
        for (i <- inj.indices)
          inj(i).asInstanceOf[Mixture.Agent].addLift(component(i), ce)

        Some(ce)
      }
      else None
    }

    /**
     * Extend a pair of pattern agents into a component embedding.
     *
     * @param u the first pattern agent in the agent pair
     * @param v the second pattern agent in the agent pair
     * @returns `Some(ce)`, where `ce` is an embedding from
     *          the pattern component containing `u` into the pattern
     *          containing `v`, or `None` if no such embedding exists.
     */
    def findEmbedding(u: Pattern.Agent, v: Pattern.Agent)
        : Option[ComponentEmbedding[Pattern.Agent]] = {

      val component = u.component
      val inj = new Array[Pattern.Agent](component.length)
      val codomain = new mutable.BitSet
      if (extendInjection(u, v, inj, codomain))
        Some(new ComponentEmbedding[Pattern.Agent](inj, component))
      else None
    }

    /**
     * Extend a multiple pattern/mixture agent pairs into a set of
     * component embeddings.
     *
     * All pattern/mixture agent pairs in `ps` must belong to the same
     * pattern component and mixture, respectively.  The component
     * embeddings returned by the method are guaranteed to be pairwise
     * unique.
     *
     * @param ps a collection of pattern/mixture agent pairs from the
     *        same pattern component and mixture, respectively.
     * @returns all the embeddings the containing any of the
     *          pairs in `ps`.
     */
    def findEmbeddings(ps: Iterable[(Pattern.Agent, Mixture.Agent)])
        : Iterable[ComponentEmbedding[Mixture.Agent]] = {
      if (ps.isEmpty) Iterable.empty
      else ps flatMap { p => findEmbedding(p._1, p._2) }
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
     * @param codomain a bit set representing indices of the agents in
     *        the codomain of the partial injection.
     * @returns `true` if the injection in `inj` is total.
     */
    private def extendInjection(
      u: Pattern.Agent, v: Pattern.Agent, inj: Array[Pattern.Agent],
      codomain: mutable.BitSet): Boolean = {
      val i = u.index
      if (inj(i) != null) inj(i) == v             // Functionality test
      else if (codomain contains v.index) false   // Injectivity test
      else {
        if (u matches v) {
          inj(i) = v
          codomain += v.index
          u.indices forall { j =>
            (u.neighbour(j), v.neighbour(j)) match {
              case (None, _) => true
              case (Some(w1), Some(w2)) => {
                extendInjection(w1, w2, inj, codomain)
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
    // FIXME1: Code duplication.  The problem is making the
    // injectivity check efficient for both Pattern and Mixture.
    // While Pattern allows us to track the codomain as a simple
    // BitSet (because we know the index of every agent), Mixture
    // provides the marking mechanism to represent subsets internally.
    //
    // FIXME2: There are lift sets in mixtures.  We can use those to
    // test for conflicts and remove the `conflicts` array.
    private def extendInjection(
      u: Pattern.Agent, v: Mixture.Agent, inj: Array[Mixture.Agent])
        : Boolean = {
      val i = u.index
      if (inj(i) != null) inj(i) == v           // Functionality test
      else if (v hasMark Visited) false         // Injectivity test
      else if (v.liftMap isDefinedAt u) false   // Duplicate test
      else {
        if (u matches v) {
          inj(i) = v
          v.mixture.mark(v, Visited)
          u.indices forall { j =>
            (u.neighbour(j), v.neighbour(j)) match {
              case (None, _) => true
              case (Some(w1), Some(w2)) =>
                extendInjection(w1, w2, inj)
              case _ => false
            }
          }
        } else false
      }
    }
  }
}
