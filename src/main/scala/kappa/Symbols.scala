package kappa

import language.postfixOps

trait Symbols {
  this: LanguageContext with Parser =>

  type Link = (AgentType, SiteName, AgentType, SiteName)
  type LinkSym = (AgentTypeSym, SiteNameSym, AgentTypeSym, SiteNameSym)

  // RHZ: @Sandro I didn't understand the way you wanted to store the symbol table
  var agentTypes         : Map[AgentTypeSym, AgentType] = Map()
  var agentTypeSyms      : Map[AgentType, AgentTypeSym] = Map()

  var siteNames          : Map[AgentTypeSym, Map[SiteNameSym, SiteName]] = Map()
  var siteNameSyms       : Map[AgentType,    Map[SiteName, SiteNameSym]] = Map()

  var agentStateNames    : Map[AgentTypeSym, Map[AgentStateNameSym, AgentStateName]] = Map()
  var agentStateNameSyms : Map[AgentType,    Map[AgentStateName, AgentStateNameSym]] = Map()

  var siteStateNames     : Map[AgentTypeSym, Map[SiteNameSym, Map[SiteStateNameSym, SiteStateName]]] = Map()
  var siteStateNameSyms  : Map[AgentType,    Map[SiteName,    Map[SiteStateName, SiteStateNameSym]]] = Map()

  // RHZ: In linkStateNames we store at the same time the information
  // about the possible links and the possible states those links can have
  var linkStateNames     : Map[LinkSym, Map[LinkStateNameSym, LinkStateName]] = Map()
  var linkStateNameSyms  : Map[Link,    Map[LinkStateName, LinkStateNameSym]] = Map()

  def initSymbols(cg: AST.ContactGraph): Unit
}

trait KappaSymbols extends Symbols {
  this: KappaContext with KappaParser =>

  def initSymbols(cg: AST.ContactGraph): Unit = {
    import AST.{CGAgent,CGSite,CGLinkAnnot}

    def mapToIndex  [A](xs: Seq[A]): Map[A, Int] = xs.zipWithIndex.toMap
    def mapFromIndex[A](xs: Seq[A]): Map[Int, A] = xs.zipWithIndex map (_.swap) toMap

    agentTypeSyms = mapToIndex(for (CGAgent(aname, _, _) <- cg) yield aname)
    agentTypes    = agentTypeSyms map (_.swap)

    agentStateNames    = Map() withDefaultValue (Map() withDefaultValue ())
    agentStateNameSyms = Map() withDefaultValue (Map() withDefaultValue ())

    siteNameSyms = (for (CGAgent(aname, _, intf) <- cg)
                    yield (aname,
                           mapToIndex(for (CGSite(sname, _, _) <- intf)
                                      yield sname))).toMap

    siteNames = (for (CGAgent(aname, _, intf) <- cg)
                 yield (agentTypeSyms(aname),
                        mapFromIndex(for (CGSite(sname, _, _) <- intf)
                                     yield sname))).toMap

    siteStateNameSyms = (for (CGAgent(aname, _, intf) <- cg)
                         yield (aname,
                                (for (CGSite(sname, sstates, _) <- intf)
                                 yield (sname,
                                        mapToIndex(sstates))).toMap)).toMap

    siteStateNames = (for (CGAgent(aname, _, intf) <- cg)
                         yield (agentTypeSyms(aname),
                                (for (CGSite(sname, sstates, _) <- intf)
                                 yield (siteNameSyms(aname)(sname),
                                        mapFromIndex(sstates))).toMap)).toMap

    val lstateMap = (for (CGLinkAnnot(lnk, lstates) <- cg)
                     yield (lnk, lstates)).toMap withDefaultValue List()

    val links = for (CGAgent(aname, _, intf) <- cg;
                     CGSite(sname, _, lnks) <- intf;
                     lnk <- lnks)
                yield (lnk, aname, sname)

    linkStateNames = links groupBy (_._1) map {
      case (lnk, List((_, aname1, sname1), (_, aname2, sname2))) =>
        ((agentTypeSyms(aname1), siteNameSyms(aname1)(sname1),
          agentTypeSyms(aname2), siteNameSyms(aname2)(sname2)),
         Map(() -> ()))
      case _ => throw new IllegalArgumentException("every bond label must appear exactly twice")
    }

    linkStateNames ++= linkStateNames map {
      case ((a1, s1, a2, s2), lstates) => ((a2, s2, a1, s1), lstates) }

    linkStateNameSyms = links groupBy (_._1) map {
      case (lnk, List((_, aname1, sname1), (_, aname2, sname2))) =>
        ((aname1, sname1, aname2, sname2), Map(() -> ()))
      case _ => throw new IllegalArgumentException("every bond label must appear exactly twice")
    }

    linkStateNameSyms ++= linkStateNameSyms map {
      case ((a1, s1, a2, s2), lstates) => ((a2, s2, a1, s1), lstates) }
  }
}

