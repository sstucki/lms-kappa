package kappa

import collection.immutable.Vector

trait Embeddings extends PartialEmbeddings {
  this: Mixtures with Patterns =>

  /**
   * A class reperesenting a total embedding as a product of
   * [[PartialEmbedding]]s.
   *
   * TODO: We are using an immutable Vector to reperesent the collection of
   * underlying [[PartialEmbeddings]], whereas KaSim uses mutable arrays and
   * constructs embeddings (aka InjProducts) by sequentially adding
   * partial embeddings (aka Injections).  Are there any advantages of the
   * KaSim method over this method?
   *
   * RHZ: This class should extend Seq, Map and perhaps PartialFunction if
   * it defines all these functions!! I mean you get extra functionality for
   * free if you do it, don't you? And conceptually an embedding is a map
   *
   * @param pes the [[PartialEmbedding]]s making up this [[Embedding]].
   * @param pattern the [[Patterns#Pattern]] that constitutes the domain of
   *        this embedding.
   * @param signature the root agents of the underlying [[PartialEmbedding]]s. 
   */
  case class Embedding(pes: Vector[PartialEmbedding],
                       pattern: Pattern,
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
