
/** Main Kappa package. */
package object kappa {
  
  // -- Type aliases for indices. --
  //
  // RHZ: Do they have to be indices? why not just identifiers?
  //
  // sstucki: I think "index" describes their function more closely.
  //          "Identifyer" is more general, that could also refer to,
  //          e.g. a name (a string) or a suple of indices.
  //
  // RHZ: That's exactly why I thought identifier would be a better name...
  //      and also because Id is shorter than Index
  //
  // sstucki: Hm, maybe we are talking about different things here.
  // These are *not* supposed to be used as identifiers, they are to
  // be used as indices of elements in sequences.  You say that the
  // 10-th element in an array has "identifier 10", it has index 10.
  // The aliases for identifiers are defined in the class Symbol.
  // Incidentally, they are end in "Sym" there, because they are not
  // arbitrary identifiers but symbols.

  /** Index of an [[Patterns#Pattern]] within a [[Model]] */
  type PatternIndex = Int

  /** Index of an [[Mixtures#Mixture]] within a [[Model]] */
  type MixtureIndex = Int

  /**
   * Index of an connected [[Patterns#Pattern.Component]] within a
   * [[Patterns#Pattern]]
   */
  type ComponentIndex = Int

  /**
   * Index of an [[Patterns#Pattern.Agent]] within a [[Patterns#Pattern]]
   */
  type AgentIndex = Int

  /**
   * Index of a [[Patterns#Pattern.Site]] or a [[Mixtures#Mixture.Site]]
   * within a [[Patterns#Pattern.Agent]] or a [[Mixtures#Mixture.Agent]]
   */
  type SiteIndex = Int

  /**
   * Index of a [[PartialEmbeddings#PartialEmbedding]] within the
   * collection of partial embeddings from a
   * [[Patterns#Pattern.Component]] to a [[Mixtures#Mixture]]
   */
  type EmbeddingIndex = Int
}
