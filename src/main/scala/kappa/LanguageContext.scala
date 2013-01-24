package kappa

/** A trait for agent states. */
trait AgentStateIntf[T] extends Matchable[T] {
  this: T =>

  /**
   * Checks if this agent state matches `that` when constructing rules
   * according to the "longest common prefix" rule.
   *
   * @return `true` if this agent state matches `that`.
   */
  def matchesInLongestCommonPrefix(that: T): Boolean
}


trait LanguageContext {
  // State types
  type AgentState <: AgentStateIntf[AgentState]
  type SiteState <: Matchable[SiteState]
  type LinkState <: Matchable[LinkState]
}


trait KappaContext extends LanguageContext {
  this: KappaSymbols =>

  type AgentState = AgentStateImpl
  type SiteState  = SiteStateImpl
  type LinkState  = LinkStateImpl

  // TODO: If we were to use dedicated symbol classes rather than Ints
  // to represent symbols, some of these wrappers would likely not be
  // necessary as we could use the symbols directly to represent the
  // states (in cases where the states are not tuples).
  case class AgentStateImpl(atype: AgentTypeSym)
      extends AgentStateIntf[AgentStateImpl] {
    override def toString = agentTypes(atype)
    def matches(that: AgentStateImpl) = this.atype == that.atype
    def isComplete = true
    def matchesInLongestCommonPrefix(that: AgentStateImpl) =
      this.atype == that.atype
  }

  case class SiteStateImpl(name: SiteNameSym, state: Option[SiteStateNameSym])
       extends Matchable[SiteStateImpl] {

         // FIXME: Adjust to new syntax?
         override def toString = siteNames(name) + (state match {
           case None => ""
           case Some(s) => "~" + siteStateNames(s)
         })

         def matches(that: SiteStateImpl) =
           (this.name == that.name) && Matchable.optionMatches(
             this.state, that.state)(_==_)

         def isComplete = !state.isEmpty
       }

  case class LinkStateImpl(state: Option[LinkStateNameSym])
       extends Matchable[LinkStateImpl] {
         override def toString = state match {
           case None => ""
           case Some(s) => ": " + linkStateNames(s)
         }

         def matches(that: LinkStateImpl) = Matchable.optionMatches(
           this.state, that.state)(_==_)

         def isComplete = !state.isEmpty
       }
}

// trait KaSpaceContext extends LanguageContext {

//   // TODO: If we use an environment (aka symbol table), then these
//   // should be references to symbols (i.e. the AgentStateId?).  If
//   // e.g. Site states are optional, it should be an option of symbol
//   // reference, etc.

//   // TODO: Again, if we use a symbol table, then these should be
//   // references to symbols, probably including the geometry!  Are
//   // there any advantages to keeping geometry as non-symbols?  I.e. in
//   // the KaSpace paper, geometry is defined to be restricted to a
//   // finite subset of radii, positions and matrices, which suggests
//   // that we might allocate symbols for them.  Is there any reason why
//   // this should not be so?
//   type Radius = Double
//   case class Position(x: Double, y: Double, z: Double)
//   type Matrix = Vector[Vector[Double]] // dummy... at some later point we should use Scalala probably

//   // FIXME: should these be proper classes?
//   type AgentState = Pair[String, Option[Radius]]
//   type SiteState  = Pair[Option[String], Option[Position]]
//   type LinkState  = Pair[Option[String], Option[Matrix]]

//   val radiusPo = partialOrderingFromEquiv(Ordering.Double)
//   val posPo = partialOrderingFromEquiv(Equiv.universal[Position])
//   val orientationPo = partialOrderingFromEquiv(Equiv.universal[Matrix])

//   val apo = productPartialOrdering(KappaContext.apo, radiusPo)
//   val spo = productPartialOrdering(KappaContext.spo, posPo)
//   val lpo = productPartialOrdering(KappaContext.lpo, orientionPo)
// }

