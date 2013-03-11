
/** Main Kappa package. */
package object kappa {

  // -- Type aliases for indices. --

  /** Index of an [[Patterns#Pattern]] within a [[Model]]. */
  type PatternIndex = Int

  /** Index of an [[Mixtures#Mixture]] within a [[Model]]. */
  type MixtureIndex = Int

  /**
   * Index of an connected [[Patterns#Pattern.Component]] within a
   * [[Patterns#Pattern]].
   */
  type ComponentIndex = Int

  /**
   * Index of an [[Patterns#Pattern.Agent]] within a [[Patterns#Pattern]].
   */
  type AgentIndex = Int

  /**
   * Index of a [[SiteGraphs#SiteGraph#Agent#Site]] within a
   * [[SiteGraphs#SiteGraph#Agent]].
   */
  type SiteIndex = Int

  /**
   * Index of a [[ComponentEmbeddings#ComponentEmbedding]] within the
   * collection of component embeddings from a
   * [[Patterns#Pattern.Component]] to a [[Mixtures#Mixture]]
   */
  type EmbeddingIndex = Int
}
