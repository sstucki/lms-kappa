
/** Main Kappa package. */
package object kappa {
  
  // -- Type aliases for indices. --
  // RHZ do they have to be indices? why not just identifiers?

  /** Index of an [[Mixture]] within a [[??]] */
  type PatternIndex = Int

  /** Index of an [[Mixture]] within a [[??]] */
  type MixtureIndex = Int

  /** Index of an connected [[Component]] within a [[??]] */
  type ComponentIndex = Int

  /** Index of an [[SiteGraphs#Agent]] within a [[SiteGraphs#SiteGraph]] */
  type AgentIndex = Int

  /** Index of a [[SiteGraphs#Site]] within a [[SiteGraphs#Agent]] */
  type SiteIndex = Int

}
