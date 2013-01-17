
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

}
