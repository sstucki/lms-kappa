package kappa

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
  type AgentState <: State[AgentState]
  type SiteState  <: State[SiteState]
  type LinkState  <: State[LinkState]

  // RHZ: We need functions to create these states.
  // Another option would be to do the evaluation of the AST
  // in the derived classes, but that doesn't make so much
  // sense since most of that code is state-agnostic
  def mkAgentState(agentType: AgentTypeSym, state: Option[AgentStateNameSym]): AgentState
  def mkSiteState(siteName: SiteNameSym, state: Option[SiteStateNameSym]): SiteState
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
  def mkSiteState(siteName: SiteNameSym, state: Option[SiteStateNameSym]) =
    SiteStateImpl(siteName, state)
  def mkLinkState(state: Option[LinkStateNameSym]) = LinkStateImpl()

  // TODO: If we were to use dedicated symbol classes rather than Ints
  // to represent symbols, some of these wrappers would likely not be
  // necessary as we could use the symbols directly to represent the
  // states (in cases where the states are not tuples).
  case class AgentStateImpl(atype: AgentTypeSym) extends State[AgentStateImpl] {
    override def toString = agentTypes(atype)
    def matches(that: AgentStateImpl) = this.atype == that.atype
    def isConcrete = true
  }
  
  case class SiteStateImpl(name: SiteNameSym, state: Option[SiteStateNameSym])
    extends State[SiteStateImpl]
  {
    // If we want to have something like the commented-out toString
    // we need a reference to the site this state belongs to
    /*
    override def toString = siteNames(name) + (state match {
      case None => ""
      case Some(s) => "~" + siteStateNames(s)
    })
    */
    override def toString = name + (state map ("~" + _) getOrElse "")

    def matches(that: SiteStateImpl) =
      (this.name == that.name) && State.matchesOption(
        this.state, that.state)(_==_)

    def isConcrete = !state.isEmpty
  }

  case class LinkStateImpl()
    extends State[LinkStateImpl]
  {
    override def toString = ""
    def matches(that: LinkStateImpl) = true
    def isConcrete = true
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

