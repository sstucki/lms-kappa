package kappa

import collection.immutable.HashMap
import collection.mutable

trait PartialEmbeddings {
  this: Mixtures =>

  /**
   * A class reperesenting an embedding form a single connected
   * component of a site graph into a mixture.
   * 
   * @param map a map representing the injection underlying this
   *            [[PartialEmbedding]].
   * @param pattern the [[PatternIndex]] of the pattern that contains
   *                the domain of this embedding.
   * @param component the [[ComponentIndex]] of the connected component
   *                  (in pattern) that conconstitutes the domain of this
   *                  embedding.
   */
  // FIXME: Replace map with vector!
  case class PartialEmbedding(map: HashMap[AgentIndex, Mixture.Agent],
                              pattern: PatternIndex,
                              component: ComponentIndex) {

    type Source = AgentIndex
    type Target = Mixture.Agent

    /** The address of this embedding in the [[EmbeddingHeap]]. */
    // FIXME: Not used.
    //var address: Option[Int] = None
    
    /**
     * Selects the first pair of this embedding.
     * 
     * *Note:* might return different results for different runs.
     *
     * @return the first pair of this embedding.
     */
    def head: (Source, Target) = map.head
    
    /**
     * Returns this [[PartialEmbedding]] as a [[Map]] from agent indices
     * to agent indices.
     *
     * FIXME: When is this used? Why not just use iterator, etc.?
     * 
     * @return this [[PartialEmbedding]] as a [[Map]] from agent indices
     * to agent indices.
     */
    def toMap: Map[Source, Target] = map

    /** What exactly is this? */
    // FIXME: Not used
    // def isTrashed: Boolean = address match {
    //   case Some(-1) => true
    //   case _ => false
    // }


    // -- Forwarding methods of the underlying Map -- 
    
    def get(key: Source) = map.get(key)

    def apply(key: Source) = map(key)

    def iterator = map.iterator

    def updated(key: Source, value: Target) =
      this.copy(map = this.map updated (key, value))
    
    def +(p: (Source, Target)) = this updated (p._1, p._2)

    def -(key: Source) = this.copy(map = this.map - key)


    /**
     * Exception used to signal a clash in an embedding, i.e. a violation
     * of the injectivity of an embedding.
     * 
     * FIXME: Don't use! See next FIXME comment of addToMap() below.
     */
    case class ClashException(map: Map[Source, Target],
                              p: (Source, Target)) extends Exception

    /**
     * This method adds the injection (map) of this embedding to an
     * aggregate injection of a composite embedding (e.g. of a pattern
     * with multiple components).
     *
     * FIXME: Don't use this method, it's only used to agregate partial
     * embeddings into total embeddings, so define a function that does
     * this directly instead.  The advantage of that other function is
     * that it can be optimized (using e.g. while or tail recursion)
     * where as this can not be.  It's also more clear what the
     * aggregation method is good for and it avoids creating and
     * catching ClashExceptions.
     *
     * @param map a map representing the aggregate injection/embedding.
     * @param cod a inv reperesenting the codomain of the agregate
     *            injection/embedding. 
     * @return a pair consisting of the updated aggregate injection and
     *         its codomain
     */
    def addToMap(map: Map[Source, Target], cod: mutable.Set[Target])
    : (Map[Source, Target], mutable.Set[Target]) = {
      (this.map foldLeft (map, cod)) { (mc, p) =>
        if (cod contains p._2) throw new ClashException(map, p)
        else (map + p, cod += p._2)
                                    }
    }
  }
}
