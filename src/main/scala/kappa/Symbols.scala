package kappa

import language.postfixOps

trait Symbols {
  this: LanguageContext with Parser =>

  // RHZ: We need this map to guarantee that sites have the same
  // order in every agent of a certain agent type (the code that
  // relies on this is in Patterns.Pattern.Agent.matches)
  // When we create the interface of an agent we have to put the
  // sites in a certain order: the one given by the vector in
  // this map
  var siteNames: Map[AgentType, Vector[SiteName]] = Map()

  def initSymbols(cg: AST.ContactGraph): Unit
}

trait KappaSymbols extends Symbols {
  this: KappaContext with KappaParser =>

  type AgentTypeSym = Int
  type SiteNameSym = Int

  type AgentStateNameSym = Unit
  type SiteStateNameSym = Int
  type LinkStateNameSym = Unit

  type Link = (AgentType, SiteName, AgentType, SiteName)

  var agentTypeSyms      : Map[AgentType, AgentTypeSym] = Map()
  var siteNameSyms       : Map[AgentType, Map[SiteName, SiteNameSym]] = Map()
  var agentStateNameSyms : Map[AgentType, Map[AgentStateName, AgentStateNameSym]] = Map()
  var siteStateNameSyms  : Map[AgentType, Map[SiteName, Map[SiteStateName, SiteStateNameSym]]] = Map()
  var linkStateNameSyms  : Map[Link, Map[LinkStateName, LinkStateNameSym]] = Map()

  def initSymbols(cg: AST.ContactGraph) {
    import AST.{CGAgent,CGSite,CGLinkAnnot}

    def mapToIndex  [A](xs: Seq[A]): Map[A, Int] = xs.zipWithIndex.toMap
    def mapFromIndex[A](xs: Seq[A]): Map[Int, A] = xs.zipWithIndex map (_.swap) toMap

    agentTypeSyms = mapToIndex(for (CGAgent(aname, _, _) <- cg) yield aname)
    agentStateNameSyms = Map() withDefaultValue (Map() withDefaultValue ())

    siteNames = (for (CGAgent(aname, _, intf) <- cg)
                 yield (aname, (for (CGSite(sname, _, _) <- intf)
                                yield sname).toVector)).toMap

    siteNameSyms = (for (CGAgent(aname, _, intf) <- cg)
                    yield (aname,
                           mapToIndex(for (CGSite(sname, _, _) <- intf)
                                      yield sname))).toMap

    siteStateNameSyms = (for (CGAgent(aname, _, intf) <- cg)
                         yield (aname,
                                (for (CGSite(sname, sstates, _) <- intf)
                                 yield (sname,
                                        mapToIndex(sstates))).toMap)).toMap

    val lstateMap = (for (CGLinkAnnot(lnk, lstates) <- cg)
                     yield (lnk, lstates)).toMap withDefaultValue List()

    val links = for (CGAgent(aname, _, intf) <- cg;
                     CGSite(sname, _, lnks) <- intf;
                     lnk <- lnks)
                yield (lnk, aname, sname)

    linkStateNameSyms = links groupBy (_._1) map {
      case (lnk, List((_, aname1, sname1), (_, aname2, sname2))) =>
        ((aname1, sname1, aname2, sname2), Map(() -> ()))
      case _ => throw new IllegalArgumentException("every bond label must appear exactly twice")
    }

    linkStateNameSyms ++= linkStateNameSyms map {
      case ((a1, s1, a2, s2), lstates) => ((a2, s2, a1, s1), lstates) }
  }
}

