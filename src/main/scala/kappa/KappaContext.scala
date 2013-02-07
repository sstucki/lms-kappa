package kappa

/** Kappa language context. */
trait KappaContext extends KappaLikeContext {
  this: KappaSymbols with Patterns with KappaActions =>

  // State type bounds
  type AgentState = KappaAgentState
  type SiteState  = KappaSiteState
  type LinkState  = KappaLinkState

  // TODO: If we were to use dedicated symbol classes rather than Ints
  // to represent symbols, some of these wrappers would likely not be
  // necessary as we could use the symbols directly to represent the
  // states (in cases where the states are not tuples).
  final case class KappaAgentState(atype: AgentTypeSym)
      extends AgentStateIntf[KappaAgentState] {

    // -- AgentStateIntf[KappaAgentState] API --

    @inline def matchesInLongestCommonPrefix(that: KappaAgentState) =
      this.atype == that.atype

    // -- Matchable[KappaAgentState] API --

    @inline def matches(that: KappaAgentState) = this.atype == that.atype

    @inline override def isEquivTo[U <: KappaAgentState](that: U): Boolean =
      this.atype == that.atype

    @inline def join(that: KappaAgentState) =
      if (this.atype == that.atype) Some(this) else None

    @inline def meet(that: KappaAgentState) =
      if (this.atype == that.atype) Some(this) else None

    @inline def isComplete = true

    // -- Any API --
    @inline override def toString = agentTypes(atype)
  }

  final case class KappaSiteState(
    name: SiteNameSym, state: Option[SiteStateNameSym])
      extends Matchable[KappaSiteState] {

    // -- Matchable[KappaSiteState] API --

    @inline def matches(that: KappaSiteState) =
      (this.name == that.name) && Matchable.optionMatches(
        this.state, that.state)(_==_)

    @inline override def isEquivTo[U <: KappaSiteState](that: U): Boolean =
      (this.name == that.name) && (this.state == that.state)

    @inline def join(that: KappaSiteState) =
      if (this.name == that.name) (this.state, that.state) match {
        case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
        case _ => Some(KappaSiteState(this.name, None))
      } else None

    @inline def meet(that: KappaSiteState) =
      if (this.name == that.name) (this.state, that.state) match {
        case (None, None) => Some(this)
        case (None, Some(s2)) => Some(that)
        case (Some(s2), None) => Some(this)
        case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
      } else None

    @inline def isComplete = !state.isEmpty

    // -- Any API --

    // FIXME: Adjust to new syntax?
    override def toString = siteNames(name) + (state match {
      case None => ""
      case Some(s) => "~" + siteStateNames(s)
    })
  }

  final case class KappaLinkState(state: Option[LinkStateNameSym])
      extends Matchable[KappaLinkState] {

    // -- Matchable[KappaLinkState] API --

    @inline def matches(that: KappaLinkState) = Matchable.optionMatches(
      this.state, that.state)(_==_)

    @inline override def isEquivTo[U <: KappaLinkState](that: U): Boolean =
      this.state == that.state

    @inline def join(that: KappaLinkState) = (this.state, that.state) match {
      case (Some(s1), Some(s2)) if s1 == s2 => Some(this)
      case _ => Some(KappaLinkState(None))
    }

    @inline def meet(that: KappaLinkState) = (this.state, that.state) match {
      case (_, None) => Some(this)
      case (None, _) => Some(that)
      case (Some(s1), Some(s2)) => if (s1 == s2) Some(this) else None
    }

    @inline def isComplete = !state.isEmpty

    // -- Any API --
    override def toString = state match {
      case None => ""
      case Some(s) => ": " + linkStateNames(s)
    }
  }
}
