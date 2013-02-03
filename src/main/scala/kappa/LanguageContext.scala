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
  // RHZ: Does it make any sense to have an extension of
  // Kappa that doesn't have agent types or site names?
  // I understand that putting everything together in a
  // "state" makes pattern matching simpler, but that
  // doesn't mean we will use a different syntax than
  // agentType[:agentState](siteName1[:siteState1][link])
  // right?
  // If we are using that syntax I think we should keep
  // the distinction between agent types and agent states
  // at the abstract level.
  // Even more, I think we should say that states are
  // always optional (here I'm talking about the raw states,
  // not the composite ones that include the agent type or
  // site name).
  // In the end do we want to write any extension for
  // which the states aren't optional?
  type AgentType
  type SiteName

  type AgentTypeSym
  // FIXME this shouldn't be fixed here but maybe we shouldn't
  // index sites in an agent by their site name either
  // Or maybe we should have a function SiteNameSym => SiteIndex
  type SiteNameSym = Int

  // State types
  type AgentStateName
  type SiteStateName
  type LinkStateName

  type AgentStateNameSym
  type SiteStateNameSym
  type LinkStateNameSym

  // Composite state types
  type AgentState <: AgentStateIntf[AgentState]
  type SiteState <: Matchable[SiteState]
  type LinkState <: Matchable[LinkState]

  // RHZ: We need functions to create these states.
  // Another option would be to do the evaluation of the AST
  // in the derived classes, but that doesn't make so much
  // sense since most of that code is state-agnostic
  def mkAgentState(agentType: AgentTypeSym, state: Option[AgentStateNameSym]): AgentState
  def mkSiteState(agentType: AgentTypeSym, siteName: SiteNameSym, state: Option[SiteStateNameSym]): SiteState
  def mkLinkState(state: Option[LinkStateNameSym]): LinkState
}


trait KappaContext extends LanguageContext {
  this: KappaSymbols =>

  type AgentType = String
  type SiteName  = String

  // TODO: Is using Ints as symbols a good choice or should we use a
  // dedicated symbol class?
  type AgentTypeSym = Int
  //type SiteNameSym  = Int

  // State types
  type AgentStateName = Unit
  type SiteStateName  = String
  type LinkStateName  = Unit

  type AgentStateNameSym = Unit
  type SiteStateNameSym  = Int
  type LinkStateNameSym  = Unit

  // Composite state types
  type AgentState = AgentStateImpl
  type SiteState  = SiteStateImpl
  type LinkState  = LinkStateImpl

  def mkAgentState(agentType: AgentTypeSym, state: Option[AgentStateNameSym]) =
    // We discard the state because Kappa doesn't have them
    AgentStateImpl(agentType)
  def mkSiteState(agentType: AgentTypeSym, siteName: SiteNameSym, state: Option[SiteStateNameSym]) =
    SiteStateImpl(agentType, siteName, state)
  def mkLinkState(state: Option[LinkStateNameSym]) = LinkStateImpl()

  // TODO: If we were to use dedicated symbol classes rather than Ints
  // to represent symbols, some of these wrappers would likely not be
  // necessary as we could use the symbols directly to represent the
  // states (in cases where the states are not tuples).
  final case class AgentStateImpl(atype: AgentTypeSym)
      extends AgentStateIntf[AgentStateImpl]
  {
    // -- AgentStateIntf[AgentStateImpl] API --

    @inline def matchesInLongestCommonPrefix(that: AgentStateImpl) =
      this.atype == that.atype

    // -- Matchable[AgentStateImpl] API --

    @inline def matches(that: AgentStateImpl) = this.atype == that.atype

    @inline override def isEquivTo[U <: AgentStateImpl](that: U): Boolean =
      this.atype == that.atype

    @inline def join(that: AgentStateImpl) =
      if (this.atype == that.atype) Some(this) else None

    @inline def meet(that: AgentStateImpl) =
      if (this.atype == that.atype) Some(this) else None

    @inline def isComplete = true

    // -- Any API --
    @inline override def toString = agentTypes(atype)
  }

  final case class SiteStateImpl(agentType: AgentTypeSym,
                                 name: SiteNameSym,
                                 state: Option[SiteStateNameSym])
      extends Matchable[SiteStateImpl]
  {
    // -- Matchable[SiteStateImpl] API --

    @inline def matches(that: SiteStateImpl) =
      (this.name == that.name) &&
      Matchable.optionMatches(this.state, that.state)(_==_)

    @inline override def isEquivTo[U <: SiteStateImpl](that: U): Boolean =
      (this.name == that.name) && (this.state == that.state)

    @inline def join(that: SiteStateImpl) =
      if (this.name == that.name) (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(SiteStateImpl(agentType, this.name, None))
      } else None

    @inline def meet(that: SiteStateImpl) =
      if (this.name == that.name) (this.state, that.state) match {
        case (None, None) => Some(this)
        case (None, Some(s2)) => Some(that)
        case (Some(s2), None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    val noStateInCG = siteStateNames(agentType)(name).isEmpty
    @inline def isComplete = noStateInCG || !state.isEmpty

    // -- Any API --

    override def toString = siteNames(agentType)(name) +
      (state map (":" + siteStateNames(agentType)(name)(_)) getOrElse "")
  }

  case class LinkStateImpl()
    extends Matchable[LinkStateImpl]
  {
    // -- Matchable[LinkStateImpl] API --
    @inline def matches(that: LinkStateImpl) = true

    @inline override def isEquivTo[U <: LinkStateImpl](that: U): Boolean = true

    @inline def join(that: LinkStateImpl) = Some(LinkStateImpl())

    @inline def meet(that: LinkStateImpl) = Some(LinkStateImpl())

    @inline def isComplete = true

    // -- Any API --
    override def toString = ""
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

