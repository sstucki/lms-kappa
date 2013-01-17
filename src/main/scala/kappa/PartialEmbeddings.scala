package kappa

import collection.immutable.HashMap
import collection.mutable

trait PartialEmbeddings {
  this: Mixtures with Patterns =>

  /**
   * A class reperesenting an embedding form a single connected
   * component of a site graph into a mixture.
   *
   * @param map an array of [[Mixtures#Mixture.Agent]]s representing
   *        the injection from pattern agent indices to mixture agents.
   * @param pattern the [[Patterns#Pattern]] of the pattern that contains
   *        the domain of this embedding.
   * @param component the connected [[Patterns#Pattern.Component]]
   *        (of `pattern`) that conconstitutes the domain of this
   *        embedding.
   */
  case class PartialEmbedding private (
    val map: Array[Mixture.Agent],
    val pattern: Pattern,
    val component: Pattern.Component) {

    type Source = AgentIndex
    type Target = Mixture.Agent

    /**
     * Returns this [[PartialEmbedding]] as a map from agent indices
     * to [[Mixtures#Mixture.Agent]]s.
     *
     * FIXME: When is this used? Why not just use iterator, etc.?
     *
     * @return this [[PartialEmbedding]] as a map from agent indices
     * to [[Mixtures#Mixture.Agent]]s.
     */
    def toMap: Map[Source, Target] = iterator.toMap

    //-- Methods imitating a Map[Source, Target] and Vector[Target] interface --
    
    /**
     * Selects the first pair of this partial embedding.
     *
     * @return the first pair of this partial embedding.
     */
    def head: (Source, Target) = (0, map.head)

    def get(key: Source): Option[Target] =
      if (key > map.size) None else Some(map(key))

    def apply(key: Source): Target = map(key)

    def iterator: Iterator[(Source, Target)] =
      for ((v, k) <- map.iterator.zipWithIndex) yield (k, v)

    def updated(key: Source, value: Target) =
      this.copy(map = this.map updated (key, value))
    
    def +(p: (Source, Target)) = this updated (p._1, p._2)

    def :+(t: Target) = this.copy(map = this.map :+ t)

    override def toString =
      "PE(" + component.index + ": " + toMap.mkString(", ") + ")"
  }

  object PartialEmbedding {

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
              extending = (k.sites(j).neighbor, v.sites(j).neighbor) match {
                case (None, _) => true
                case (Some(s1), Some(s2)) =>
                  (s1 == s2) && extend(s1.agent.index, s2.agent)
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
