package kappa

trait ContactGraph {
  this: LanguageContext with Parser =>

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
    override def toString: String = contactGraphString
  }

  private var _contactGraph: ContactGraph = null

  def contactGraph: ContactGraph =
    if (_contactGraph != null) _contactGraph
    else throw new IllegalStateException("no contact graph defined")

  def contactGraph_= (cg: String) = {
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

  /** Find an agent state in the contact graph. */
  def findAgentStateSet(agentState: AgentStateName): AgentStateSet

  /** Find a site state in the contact graph. */
  def findSiteStateSet(agentStateSet: AgentStateSet,
                       siteState: SiteStateName): SiteStateSet

  /** Find a link state in the contact graph. */
  def findLinkStateSet(source: SiteStateSet,
                       target: Option[SiteStateSet],
                       linkState: LinkStateName): LinkStateSet
}

trait KappaLikeContactGraph extends ContactGraph {
  this: KappaLikeContext with KappaLikeParser =>

  /** Find an agent state in the contact graph. */
  def findAgentStateSet(agentState: AgentStateName): AgentStateSet =
    contactGraph.agentStateSets.find { agentStateSet =>
      agentState.agentType == agentStateSet.agentType
    } match {
      case Some(agentStateSet) => agentStateSet
      case None => throw new IllegalArgumentException(
        "couldn't find agent state set for " + agentState)
    }

  /** Find a site state in the contact graph. */
  def findSiteStateSet(agentStateSet: AgentStateSet,
                       siteState: SiteStateName): SiteStateSet =
    contactGraph.siteStateSets.find { siteStateSet =>
      (siteState.siteName == siteStateSet.siteName) &&
      (agentStateSet == siteStateSet.agentStateSet)
    } match {
      case Some(siteStateSet) => siteStateSet
      case None => throw new IllegalArgumentException(
        "couldn't find site state set for " + siteState)
    }
}

trait KappaContactGraph extends KappaLikeContactGraph {
  this: KappaContext with KappaParser =>

  /** Find a link state in the contact graph. */
  def findLinkStateSet(source: SiteStateSet,
                       target: Option[SiteStateSet],
                       linkState: LinkStateName): LinkStateSet =
    KappaLinkStateSet
}

trait KaSpaceContactGraph extends KappaLikeContactGraph {
  this: KaSpaceContext with KaSpaceParser =>

  /** Find a link state in the contact graph. */
  def findLinkStateSet(source: SiteStateSet,
                       target: Option[SiteStateSet],
                       linkState: LinkStateName): LinkStateSet =
    contactGraph.linkStateSets.find { linkStateSet =>
      (linkStateSet.source == source) &&
      (target map (linkStateSet.target == _) getOrElse true) &&
      (linkState.label map (w1 =>
        linkStateSet.labels exists (w2 => w1 ~= w2)) getOrElse true)
    } match {
      case Some(agentStateSet) => agentStateSet
      case None => throw new IllegalArgumentException(
        "couldn't find link state set from " + source +
        " to " + target + " with state " + linkState)
    }
}

