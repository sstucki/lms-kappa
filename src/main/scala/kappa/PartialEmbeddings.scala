package kappa

trait PartialEmbeddings {
  this: Mixtures with Patterns =>

  /**
   * A class reperesenting an embedding form a single connected
   * component of a site graph into a mixture.
   *
   * WARNING: For convenience, this class provides the interface of a
   * `Seq[Mixtures#Mixture.Agent]`.  However, using some methods from
   * the `Seq` API might not result in the expected behavior.
   * E.g. `++` will return a `Seq[PartialEmbedding]` rather than the
   * expected `PartialEmbedding`.
   *
   * @param map an array of [[Mixtures#Mixture.Agent]]s representing
   *        the injection from pattern agent indices to mixture
   *        agents.
   * @param pattern the [[Patterns#Pattern]] of the pattern that contains
   *        the domain of this embedding.
   * @param component the connected [[Patterns#Pattern.Component]]
   *        (of `pattern`) that conconstitutes the domain of this
   *        embedding.
   */
  case class PartialEmbedding private (
    val map: Array[Mixture.Agent],
    val pattern: Pattern,
    val component: Pattern.Component)
      extends Seq[Mixtures#Mixture.Agent] {

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
    @inline def head: (Source, Target) = (0, map.head)

    @inline def get(key: Source): Option[Target] =
      if (key > map.size) None else Some(map(key))

    @inline def mapIterator: Iterator[(Source, Target)] =
      for ((v, k) <- map.iterator.zipWithIndex) yield (k, v)

    // TODO: Potentially confusing?
    @inline def +(p: (Source, Target)) = this updated (p._1, p._2)


    // -- Seq[Target] API --

    @inline def apply(idx: Source): Target = map(key)

    @inline def iterator: Iterator[Target] = map.iterator

    @inline def :+(elem: Target) = this.copy(map = this.map :+ elem)

    @inline def updated(index: Source, elem: Target) =
      this.copy(map = this.map updated (index, elem))


    override def toString =
      "PE(" + component.index + ": " + toMap.mkString(", ") + ")"
  }

  object PartialEmbedding {

    /**
     * Extend a pattern/mixture agent pair into a partial embedding.
     *
     * @param k the pattern agent in the agent pair
     * @param v the mixture agent in the agent pair
     * @returns `Some(pe)`, where `pe` is a (partial) embedding from
     *          the pattern containing `k` into the mixture containing
     *          `v`, or `None` if no such embedding exists.
     */
    def apply(k: Pattern.Agent, v: Mixture.Agent): Option[PartialEmbedding] = {
      val component = k.component
      val map = new Array[Mixture.Agent](component.length)
      def extend(i: AgentIndex, v: Mixture.Agent): Boolean = {
        if (map(i) != null) map(i) == v
        else {
          val k = component(i)
          if (k matches v) {
            map(i) = v
            val n = k.sites.size
            var j: Int = 0
            var extending: Boolean = true
            while (j < n && extending) {
              extending = (k.neighbor(j), v.neighbor(j)) match {
                case (None, _) => true
                case (Some((a1, s1)), Some((a2, s2))) =>
                  (s1 == s2) && extend(a1.index, a2)
                case _ => false
              }
              j += 1
            }
            extending
          } else false
        }
      }

      if (extend(k.index, v)) Some(
        new PartialEmbedding(map, k.component.pattern, k.component))
      else None
    }
  }
}
