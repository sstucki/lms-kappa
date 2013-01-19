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
   * sstucki: Well, yes and no.  It's true that you get extra functionality
   * but it might not do what you expect it to.  For example, assume you
   * were to extend Seq, then you get sequence concatenation (`++`) out of
   * the box, but `e1 ++ e1` won't return an embedding even when both `e1`
   * and `e2` are embeddings!  The reason is that the collections library
   * uses builders for stuff like sequence concatenation and we have not
   * define a builder for embeddings.  To conclude: yes we should probably
   * make this proper Maps at some point, but it will require some effort.
   * PatialFunction, on the other hand, seems like a low-hanging fruit :-)
   *
   * sstucki: UPDATE: I've been thinking and it's probably rather
   * tricky to develop and actual builder for the graph structures
   * (see the documentation in [[Mixturres#Mixture]].  For the
   * embeddings, it's different, because we're really just wrapping a
   * vector so we could, at least in principle, write an appropriate
   * builder (I think one can even re-use exiting builders to some
   * degree).  For now, we can just extend these classes with the
   * appropriate Seq and put a warning there, e.g. like this:
   *
   * WARNING: For convenience, this class provides the interface of a
   * `Seq[PartialEmbedding]`.  However, using some methods from the
   * `Seq` API might not result in the expected behavior.  E.g. `++`
   * will return a `Seq[PartialEmbedding]` rather than the expected
   * `Embedding`.
   *
   * For many use cases this is enough and if the expected behavior is
   * ever required (e.g. `++` returning an Embedding), we can always
   * implement it later.  Does that that sound like a good compromise?
   *
   * @param pes the [[PartialEmbedding]]s making up this [[Embedding]].
   * @param pattern the [[Patterns#Pattern]] that constitutes the domain of
   *        this embedding.
   * @param signature the root agents of the underlying [[PartialEmbedding]]s.
   */
  case class Embedding(
    pes: Vector[PartialEmbedding],
    pattern: Pattern,
    signature: Vector[AgentIndex])
      extends Seq[PartialEmbedding] {

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
