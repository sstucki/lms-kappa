package kappa

trait Symbols {
  // FIXME: Should we pass in an instance of Parser rather than make
  // it a dependency?
  this: Parser =>

  // FIXME: This should set up the symbol tables (vectors/maps)
  //def initSymbols(cg: AST.ContactGraph): Unit
}

trait KappaSymbols extends Symbols {
  this: KappaContext with KappaParser =>

  // TODO: Is using Ints as symbols a good choice or should we use a
  // dedicated symbol class?
  type AgentType         = String
  type SiteName          = String
  type SiteStateName     = String
  type LinkStateName     = String
  type AgentTypeSym      = Int
  type SiteNameSym       = Int
  type SiteStateNameSym  = Int
  type LinkStateNameSym  = Int

  // In linkstate we store at the same time the information about
  // the allowed links and the possible states those links can have
  var agentTypes: Vector[AgentType] = Vector()
  var siteNames: Vector[SiteName] = Vector()
  var siteStateNames: Vector[SiteStateName] = Vector()
  var linkStateNames: Vector[LinkStateName] = Vector()
  var agentTypeSyms: Map[AgentType, AgentTypeSym] = Map()
  var siteNameSyms: Map[SiteName, SiteNameSym] = Map()
  var siteStateNameSyms: Map[SiteStateName, SiteStateNameSym] = Map()
  var linkStateNameSyms: Map[LinkStateName, LinkStateNameSym] = Map()

  // FIXME: This should set up the vectors/maps above
  //def initSymbols(cg: AST.ContactGraph) = {
    // lazy val nats : Stream[Int] = Stream.cons(0, nats.map{_ + 1})

    // import AST.{CGAgent,CGSite,CGLinkAnnot}

    // val anames = for (CGAgent(aname, _, _) <- cg) yield aname
    // val snames = for (CGAgent(_, _, intf) <- cg)
    //              yield for (CGSite(sname, _, _) <- intf) yield sname

    // val agentnameId = Map(anames zip nats:_*)
    // val sitenameId = Map(anames zip (snames map (xs => Map(xs zip nats:_*))):_*)

    // val astates = for (CGAgent(_, astates, _) <- cg) yield astates.to[Vector]
    // val astateId = Map( (for (CGAgent(aname, astates, _) <- cg) yield (aname, Map(astates zip nats:_*))) :_*)

    // val sstates = for (CGAgent(_, _, intf) <- cg)
    //               yield for (CGSite(_, sstates, _) <- intf) yield sstates.to[Vector]
    // val sstateId = Map( (for (CGAgent(aname, _, intf) <- cg)
    //                      yield (aname, Map( (for (CGSite(sname, sstates, _) <- intf)
    //                                          yield (sname, Map(sstates zip nats:_*))) :_*))) :_*)

    // val links = for (CGAgent(aname, _, intf) <- cg; CGSite(sname, _, lnks) <- intf; lnk <- lnks)
    //             yield (lnk, agentnameId(aname), sitenameId(aname)(sname))
    // val lstates = Map( (for (CGLinkAnnot(lnk, lstates) <- cg) yield (lnk, lstates)) :_*) withDefaultValue List()
    // val linkstateHalf = links groupBy (_._1) map {
    //   case (lnk, (_, a1, s1) :: (_, a2, s2) :: Nil) => ((a1, s1, a2, s2), lstates(lnk).to[Vector])
    //   case _ => throw new IllegalArgumentException("every bond label must appear exactly twice")
    // }
    // val linkstate = linkstateHalf ++ linkstateHalf map { case ((a1, s1, a2, s2), lstates) => ((a2, s2, a1, s1), lstates) }

    // new Env(agentnameId, anames.to[Vector], astates.to[Vector], astateId,
    //         sitenameId,  snames.to[Vector] map (_.to[Vector]), sstates.to[Vector] map (_.to[Vector]), sstateId,
    //         linkstate, linkstate mapValues (lstates => Map(lstates zip nats:_*)))
    // // FIXME what do we do about the directed link states issue?
  //}
}

