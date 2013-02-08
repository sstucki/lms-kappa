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

  var agentTypeSyms      : Map[AgentType, AgentTypeSym] = Map()
  var siteNameSyms       : Map[AgentType, Map[SiteName, SiteNameSym]] = Map()
  var agentStateNameSyms : Map[AgentType, Map[AgentStateName, AgentStateNameSym]] = Map()
  var siteStateNameSyms  : Map[AgentType, Map[SiteName, Map[SiteStateName, SiteStateNameSym]]] = Map()
  var linkStateNameSyms  : Map[Link, Map[LinkStateName, LinkStateNameSym]] = Map()

  def initSymbols(cg: AST.ContactGraph) {
    import AST.{CGAgent,CGSite,CGLinkAnnot}

    def mapToIndex  [A](xs: Seq[A]): Map[A, Int] = xs.zipWithIndex.toMap
    def mapFromIndex[A](xs: Seq[A]): Map[Int, A] = xs.zipWithIndex map (_.swap) toMap

    agentTypeSyms = mapToIndex(for (CGAgent(atype, _, _) <- cg) yield atype)
    agentStateNameSyms = Map() withDefaultValue (Map() withDefaultValue ())

    siteNames = (for (CGAgent(atype, _, intf) <- cg)
                 yield (atype, (for (CGSite(sname, _, _) <- intf)
                                yield sname).toVector)).toMap

    siteNameSyms = (for (CGAgent(atype, _, intf) <- cg)
                    yield (atype,
                           mapToIndex(for (CGSite(sname, _, _) <- intf)
                                      yield sname))).toMap

    siteStateNameSyms = (for (CGAgent(atype, _, intf) <- cg)
                         yield (atype,
                                (for (CGSite(sname, sstates, _) <- intf)
                                 yield (sname,
                                        mapToIndex(sstates))).toMap)).toMap

    val lstateMap = (for (CGLinkAnnot(lnk, lstates) <- cg)
                     yield (lnk, lstates)).toMap withDefaultValue List()

    val links = for (CGAgent(atype, _, intf) <- cg;
                     CGSite(sname, _, lnks) <- intf;
                     lnk <- lnks)
                yield (lnk, atype, sname)

    linkStateNameSyms = links groupBy (_._1) map {
      case (lnk, List((_, atype1, sname1), (_, atype2, sname2))) =>
        ((atype1, sname1, atype2, sname2), Map(() -> ()))
      case _ => throw new IllegalArgumentException(
        "every bond label must appear exactly twice")
    }

    linkStateNameSyms ++= linkStateNameSyms map {
      case ((a1, s1, a2, s2), lstates) => ((a2, s2, a1, s1), lstates)
    }
  }
}

trait KaSpaceSymbols extends Symbols {
  this: KaSpaceContext with KaSpaceParser =>

  type AgentTypeSym = Int
  type SiteNameSym = Int

  var agentTypeSyms      : Map[AgentType, AgentTypeSym] = Map()
  var siteNameSyms       : Map[AgentType, Map[SiteName, SiteNameSym]] = Map()
  // RHZ: For proper site graph type-checking I need more than this
  var hasAgentStateNames : Map[AgentType, Boolean] = Map()
  var hasSiteStateNames  : Map[AgentType, Map[SiteName, Boolean]] = Map()
  var hasLinkStateNames  : Map[Link, Boolean] = Map()

  def initSymbols(cg: AST.ContactGraph) {
    import AST.{CGAgent,CGSite,CGLinkAnnot}

    def mapToIndex  [A](xs: Seq[A]): Map[A, Int] = xs.zipWithIndex.toMap
    def mapFromIndex[A](xs: Seq[A]): Map[Int, A] = xs.zipWithIndex map (_.swap) toMap

    agentTypeSyms = mapToIndex(for (CGAgent(atype, _, _) <- cg) yield atype)

    siteNames = (for (CGAgent(atype, _, intf) <- cg)
                 yield (atype, (for (CGSite(sname, _, _) <- intf)
                                yield sname).toVector)).toMap

    siteNameSyms = (for (CGAgent(atype, _, intf) <- cg)
                    yield (atype,
                           mapToIndex(for (CGSite(sname, _, _) <- intf)
                                      yield sname))).toMap

    hasAgentStateNames = (for (CGAgent(atype, astates, _) <- cg)
                          yield (atype, astates.isEmpty)).toMap

    hasSiteStateNames = (for (CGAgent(atype, _, intf) <- cg)
                         yield (atype, (for (CGSite(sname, sstates, _) <- intf)
                                        yield (sname, sstates.isEmpty)).toMap)).toMap

    val lstateMap = (for (CGLinkAnnot(lnk, _) <- cg)
                     yield (lnk, true)).toMap withDefaultValue false

    val links = for (CGAgent(atype, _, intf) <- cg;
                     CGSite(sname, _, lnks) <- intf;
                     lnk <- lnks)
                yield (lnk, atype, sname)

    hasLinkStateNames = links groupBy (_._1) map {
      case (lnk, List((_, atype1, sname1), (_, atype2, sname2))) =>
        ((atype1, sname1, atype2, sname2), lstateMap(lnk))
      case _ => throw new IllegalArgumentException(
        "every bond label must appear exactly twice")
    }

    hasLinkStateNames ++= hasLinkStateNames map {
      case ((a1, s1, a2, s2), x) => ((a2, s2, a1, s1), x)
    }
  }
}

