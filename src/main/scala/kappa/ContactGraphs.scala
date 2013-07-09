package kappa

trait ContactGraphs {
  this: LanguageContext with Parsers =>

  /** Contact graph.
   *
   * NOTE: For now they are just a collection of agent, site and link state
   * sets, but perhpas in the future they can resemble more Pattern. In other
   * words, this is a bit hacky for now and should be made more elegant in
   * the future.
   */
  class ContactGraph(
    val agentStateSets: Vector[AgentStateSet],
    val siteStateSets: Vector[SiteStateSet],
    val linkStateSets: Vector[LinkStateSet],
    private val contactGraphString: String)
  {
    // TODO: Define contact graph extension
    // def +=(cg: String) { }

    override def toString: String = contactGraphString
  }

  private var _contactGraph: ContactGraph = null

  def contactGraph: ContactGraph =
    if (_contactGraph != null) _contactGraph
    else throw new IllegalStateException("no contact graph defined")

  def contactGraph_= (cg: String) = {
    if (_contactGraph != null)
      throw new IllegalStateException(
        "contact graph cannot be redefined from scratch")

    val ast = parseContactGraph(cg)
    val cgb = new ContactGraphBuilder
    cgb.contactGraphString = cg

    for (agent <- ast)
      cgb += agent

    _contactGraph = cgb.result
  }

  private class ContactGraphBuilder // extends Builder[...]
  {
    var agentStateSets: Vector[AgentStateSet] = Vector()
    var siteStateSets: Vector[SiteStateSet] = Vector()
    var linkStateSets: Vector[LinkStateSet] = Vector()
    var contactGraphString: String = ""
    var pairs: Map[LinkId, List[(SiteStateSet, LinkStateSetName)]] =
      Map() withDefaultValue List()

    def += (agent: AST.CGAgent) = agent match {
      case AST.CGAgent(agentStateSet, intf) => {
        val newASS = mkAgentStateSet(agentStateSet)
        agentStateSets = agentStateSets :+ newASS
        for (AST.CGSite(siteStateSet, linkStateSets) <- intf) {
          val newSSS = mkSiteStateSet(newASS, siteStateSet)
          siteStateSets = siteStateSets :+ newSSS
          for (linkStateSet <- linkStateSets)
            pairs += (linkStateSet.id ->
                       ((newSSS, linkStateSet) :: pairs(linkStateSet.id)))
        }
        /*
        siteStateSets ++= (for (AST.CGSite(siteStateSet, _) <- intf)
                           yield mkSiteStateSet(agentStateSet, siteStateSet))
        linkStateSets ++= (for (AST.CGSite(_, linkStateSets) <- intf;
                                linkStateSet <- linkStateSets)
                           yield mkLinkStateSet(linkStateSet))
        */
      }
    }

    def result: ContactGraph = {
      pairs foreach {
        case (_, List((s2, l2), (s1, l1))) =>
          linkStateSets = linkStateSets :+
            mkLinkStateSet(s1, s2, l1) :+
            mkLinkStateSet(s2, s1, l2)

        case _ => throw new IllegalArgumentException(
          "every bond label must appear exactly twice in contact graph")
      }
      new ContactGraph(agentStateSets, siteStateSets, linkStateSets,
                       contactGraphString)
    }
  }
}

