package kappa

import collection.immutable.Vector


trait Embeddings extends ComponentEmbeddings {
  this: ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures =>

  /**
   * A class reperesenting a total embedding as a product of
   * [[ComponentEmbedding]]s.
   *
   * ''WARNING'': For convenience, this class provides the interface
   * of a `Seq[ComponentEmbedding]`.  However, using some methods from
   * the `Seq` API might not result in the expected behavior.
   * E.g. `++` will return a `Seq[ComponentEmbedding]` rather than the
   * expected `Embedding`.
   *
   * For many use cases this is enough and if the expected behavior is
   * ever required (e.g. `++` returning an Embedding), we can always
   * implement it later.  Does that that sound like a good compromise?
   *
   * @param T the target agent type.
   * @param pes the [[ComponentEmbedding]]s making up this [[Embedding]].
   * @param pattern the [[Patterns#Pattern]] that constitutes the domain of
   *        this embedding.
   */
  final class Embedding[T <: SiteGraph#AgentIntf](
    val ces: Array[ComponentEmbedding[T]],
    val pattern: Pattern)
      extends Seq[ComponentEmbedding[T]] {

    /**
     * Return the image of the root agent of a given component
     * embedding.
     *
     * @param idx index of the component embedding to return the image
     *        of the root agent from.
     * @param the image of the root agent of the component embedding at
     *        index `idx`.
     */
    @inline def signature(idx: ComponentIndex) = ces(idx)(0)


    // -- Core Seq[ComponentEmbedding] API --

    @inline def apply(idx: ComponentIndex) = ces(idx)

    @inline def iterator = ces.iterator

    @inline def length: Int = ces.length


    // -- Extra Seq[ComponentEmbedding] API --

    @inline override def foreach[U](f: ComponentEmbedding[T] => U)
        : Unit = ces foreach f
  }
}
