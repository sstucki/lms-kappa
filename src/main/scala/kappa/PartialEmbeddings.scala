package kappa

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
  final case class PartialEmbedding /*private*/ (
    val inj: Array[Mixture.Agent], val component: Pattern.Component)
      extends Seq[Mixture.Agent] {

    type Source = AgentIndex
    type Target = Mixture.Agent

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


    override def toString =
      "PE(" + component.index + ": " + toMap.mkString(", ") + ")"
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
      def extend(i: AgentIndex, v: Mixture.Agent): Boolean = {
        if (inj(i) != null) inj(i) == v
        else {
          val u = component(i)
          if (u matches v) {
            inj(i) = v
            (0 until u.sites.size) forall { j =>
              (u.neighbor(j), v.neighbor(j)) match {
                case (None, _) => true
                case (Some((a1, s1)), Some((a2, s2))) =>
                  (s1 == s2) && extend(a1.index, a2)
                case _ => false
              }
            }
          } else false
        }
      }

      if (extend(u.index, v)) Some(
        new PartialEmbedding(inj, u.component))
      else None
    }
  }
}
