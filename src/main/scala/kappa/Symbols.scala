package kappa

/*
import scala.collection.{mutable => m}

trait Symbols {
  this: LanguageContext with Parser =>

  /**
   * This method allows us to order sites in a consistent way
   * throughout the interface of all agents of the same type.
   *
   * FIXME This method is not enough!!
   *
   * NOTE: The code that relies on the assumption that sites
   * need to be ordered is in Patterns.Pattern.Agent.matches.
   */
  //def siteIndex(agentState: AgentStateName, siteState: SiteStateName): SiteIndex

  //def undefinedInterface

  /** This method writes the symbol table from a given contact graph. */
  def initSymbols(cg: AST.ContactGraph): Unit
}

trait KappaLikeSymbols {
  this: KappaLikeContext with KappaLikeParser =>

  type AgentTypeSym = Int
  type SiteNameSym = Int

  var agentTypeSyms: Map[AgentType, AgentTypeSym] = Map()
  var siteNameSyms: Map[SiteName, SiteNameSym] = Map()

  @inline final protected def mapToIndex[A](xs: Seq[A]): Map[A, Int] =
    xs.zipWithIndex.toMap

  def initSymbols(cg: AST.ContactGraph) {
    import AST.{CGAgent,CGSite}

    val atypes: m.ListBuffer[AgentType] = new m.ListBuffer()

    for (CGAgent(astate, intf) <- cg) {
      atypes += astate.atype

      val snames: m.ListBuffer[SiteName] = new m.ListBuffer()

      for (CGSite(sstate, links) <- intf)
        snames += sstate.sname

      // TODO Should I make this a builder too?
      siteNameSyms += ((astate.atype, mapToIndex(snames.result)))
    }

    agentTypeSyms = mapToIndex(atypes.result)
  }
}
*/
