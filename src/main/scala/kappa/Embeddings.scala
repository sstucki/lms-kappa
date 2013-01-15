package kappa

import collection.immutable.Vector

trait Embeddings extends PartialEmbeddings {
  this: Mixtures =>

  /**
   * A class reperesenting a total embedding as a product of
   * [[PartialEmbedding]]s.
   *
   * TODO: We are using an immutable Vector to reperesent the collection of
   * unerlying [[PartialEmbeddings]], whereas KaSim uses mutable arrays and
   * constructs embeddings (aka InjProducts) by sequentially adding
   * partial embeddinsg (aka Injections).  Are there any advantages of the
   * KaSim method over this method?
   * 
   * @param pes the [[PartialEmbedding]] making up this [[Embedding]].
   * @param pattern the [[PatternIndex]] of the pattern that constitutes
   *                the domain of this embedding.
   * @param signature the root agents of the underlying [[PartialEmbedding]]s. 
   */
  case class Embedding(pes: Vector[PartialEmbedding],
                       pattern: PatternIndex,
                       signature: Vector[AgentIndex]) {

    // -- Forwarding methods of the underlying Vectors -- 

    def apply(key: ComponentIndex) = pes(key)

    def iterator = pes.iterator

    def updated(key: ComponentIndex, value: PartialEmbedding) =
      this.copy(pes = this.pes updated (key, value),
                signature = this.signature updated (key, value.head._1))
    
    def :+(pe: PartialEmbedding) =
      this.copy(pes = this.pes :+ pe, signature = this.signature :+ pe.head._1)

    def length = pes.length

    def size = pes.length
  }
}
