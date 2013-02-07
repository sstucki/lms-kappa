package kappa

/** Generic language context trait. */
trait LanguageContext {
  this: Patterns with Actions =>

  // State type bounds
  type AgentState <: Matchable[AgentState]
  type SiteState  <: Matchable[SiteState]
  type LinkState  <: Matchable[LinkState]
}

/** Language context for Kappa-like languages. */
trait KappaLikeContext extends LanguageContext {
  this: Patterns with Actions =>

  /** A trait for agent states. */
  trait AgentStateIntf[T] extends Matchable[T] {
    this: T =>

    /**
     * Checks if this agent state matches `that` when constructing
     * rules according to the "longest common prefix" rule.
     *
     * @return `true` if this agent state matches `that`.
     */
    def matchesInLongestCommonPrefix(that: T): Boolean
  }

  // Refined type bound for agent state type
  type AgentState <: AgentStateIntf[AgentState]
}
