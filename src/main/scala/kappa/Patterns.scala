package kappa

import scala.language.postfixOps

trait Patterns {
  this: LanguageContext with Parser with Rules with Symbols with Mixtures =>

  /**
   * A class representing patterns in [[kappa.Model]]s (i.e. site graphs).
   *
   * @constructor create a new Pattern.
   * @param components the connected components of this pattern.
   * @param agents a vector of [[Agent]]s that can be used to query
   *        the individual agents in this pattern given their
   *        position within the pattern (i.e. in the order in which
   *        they were added to the pattern).
   * @param siteGraphString the string associated with this pattern
   *        (FIXME: what if this string does not exist?) 
   */
  class Pattern private (val components: Vector[Pattern.Component],
                         val agents: Vector[Pattern.Agent],
                         val siteGraphString: String) {
    import Pattern._

    /**
     * Return the (overestimated) number of matchings of this pattern
     * in the target mixture
     */
    def count: Double = (components map (_.count)).foldLeft(1.0)(_*_)

    /**
     * Return a new pattern extending this pattern with an additional
     * (unconnected) agent.
     *
     * @param agent the agent to add.
     * @return a new pattern extending this pattern with an additional
     *         (unconnected) agent.
     */
    def :+(agent: Agent): Pattern = new Pattern(
      components :+ new Component(Vector(agent)),
      agents :+ agent, siteGraphString)

    /**
     * Return a new pattern connecting two unconnected sites from this
     * pattern.
     *
     * If the two sites do not belong to the same connected component
     * of this pattern, the two respecive components will be merged.
     * 
     * FIXME: Merging is O(n) in the size of the pattern...
     *
     * @param agent the agent to add.
     * @return a new pattern extending this pattern with an additional
     *         (unconnected) agent.
     */
    def connect(a1: AgentIndex, s1: SiteIndex, l1: LinkState,
                a2: AgentIndex, s2: SiteIndex, l2: LinkState): Pattern = {
      val u1 = agents(a1)
      val u2 = agents(a2)
      val c1 = u1.component
      val c2 = u2.component
      u1.sites(s1).link = Linked(u2, s2, l1)
      u2.sites(s2).link = Linked(u1, s1, l2)
      if (c1 != c2) merge(c1, c2) else this
    }

    /**
     * Return a new pattern where two components have been merged.
     *
     * FIXME: This is O(n) in the size of the pattern...
     */
    @inline private def merge(c1: Component, c2: Component): Pattern = {
      val (k1, k2) = if (c1.index < c2.index) (c1.index, c2.index)
                     else (c2.index, c1.index)
      val as1 = components(k1).agents
      val as2 = components(k2).agents
      val c = new Component(as1 ++ as2)
      val cs =
        (components updated (k1, c) take (k2)) ++ (components drop (k2 + 1))
      new Pattern(cs, agents, siteGraphString)
    }

    override def toString =
      if (!siteGraphString.isEmpty) siteGraphString
      else iterator.mkString("", ",", "")

    // Action constructor
    // FIXME: Should delegate to a factory method in Action object instead.
    def -> (rhs: Pattern) = new Action(this, rhs)

    // Seq methods
    @inline def apply(idx: Int): Agent = agents(idx)
    @inline def iterator: Iterator[Agent] = agents.iterator
    @inline def length: Int = agents.length

    @inline def apply(ci: ComponentIndex, ai: AgentIndex): Agent =
      components(ci).agents(ai)

    /** Update the pattern pointers in the components. */
    private def registerComponents {
      var i = 0
      for (c <- components) {
        c.pattern = this
        c.index = i
        i += 1
      }
    }

    registerComponents
  }

  /** Companion object of the [[Patterns.Pattern]] class. */
  object Pattern {

    /** A class representing links between [[Pattern.Site]]s. */
    sealed abstract class Link {

      /**
       * Compare this link against a [[Mixture.Link]].
       *
       * @return `true` if this site matches `that`.
       */
      def matches(that: Mixture.Link): Boolean = (this, that) match {
        case (Undefined, _) => true
        case (Stub, Mixture.Stub) => true
        case (Wildcard(a1, s1, l1), Mixture.Linked(a2, s2, l2)) => (
          a1 match {
            case Some(a1) => apo.lteq(a2.state, a1)
            case None => true
          }) && (s1 match {
          case Some(s1) => spo.lteq(a2.sites(s2).state, s1)
          case None => true
        }) && (l1 match {
            case Some(l1) => lpo.lteq(l2, l1)
            case None => true
          })
        case (Linked(_, _, l1), Mixture.Linked(_, _, l2)) => {
          lpo.lteq(l1, l2)
        }
        case _ => false
      }

      // FIXME!
      override def toString = this match {
        case Undefined => "?"
        case Wildcard(a, s, l) => "!" + (
          a match {
            case Some(a) => a
            case None => "_"
          }) + "." + (s match {
          case Some(s) => s
          case None => "_"
        }) + (l match {
            case Some(l) => l
            case None => "_"
          })
        case Stub => ""
        case Linked(a, s, l) =>
          "!" + a.state + "." + a.sites(s).state + l
      }
    }
    case object Undefined extends Link
    case object Stub extends Link
    case class Linked(agent: Agent, site: SiteIndex,
                      state: LinkState) extends Link
    case class Wildcard(agentState: Option[AgentState],
                        siteState: Option[SiteState],
                        linkState: Option[LinkState]) extends Link

    /**
     * A class representing sites in [[Pattern.Agent]]s.
     */
    class Site private (val state: SiteState, var link: Link = Undefined) {

      /** Convenience copy method. */
      def copy(state: SiteState = this.state, link: Link = this.link) =
        new Site(state, link match {
          case l: Linked => throw new IllegalArgumentException(
            "attempt to copy an linked site!") // don't duplicate links!
          case l: Link => l 
        })

      /**
       * Compare this site against a [[Mixture.Site]].
       *
       * @return `true` if this site matches `that`.
       */
      def matches(that: Mixture.Site): Boolean = {
        if (spo.lteq(that.state, state)) link matches that.link else false
      }

      override def toString = state.toString + link
    }

    object Site {
      def apply(state: SiteState) = new Site(state)
      def apply(state: SiteState, stub: Boolean) =
        new Site(state, if (stub) Stub else Undefined)
      def apply(state: SiteState, wildcard: Wildcard) = new Site(state, wildcard)
      def unapply(site: Site) = (site.state, site.link)
    }

    /**
     * A class representing agents in [[Pattern.Component]]s.
     *
     * @param index the index of this agent in the pattern it belongs to.
     */
    case class Agent(val state: AgentState, val sites: Vector[Site]) {

      /** The component this agent belongs to. */
      var component: Component = null

      /** The index of the agent within the component. */
      var index: AgentIndex = 0

      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @param site the index of the site whose neighbor we try to find.
       * @return `Some(x)`, where `x` is the neighbor of this site, if
       *         this site is not connected, and `None` otherwise.
       */
      @inline def neighbor(site: SiteIndex): Option[(Agent, SiteIndex)] =
        sites(site).link match {
          case Linked(a, s, _) => Some((a, s))
          case _ => None
        }

      override def toString() = state + sites.mkString("(", ",", ")")

      // Seq methods
      @inline def apply(idx: Int): Site = sites(idx)
      @inline def iterator: Iterator[Site] = sites.iterator
      @inline def length: Int = sites.length

      // Register sites.
      //for (s <- sites) s.agent = this
    }

    /**
     * A class representing connected components of [[Patterns.Pattern]]s
     * (i.e. connected components of site graphs).
     */
    class Component(val agents: Vector[Agent]) {

      /** The pattern this component belongs to. */
      var pattern: Pattern = null

      /** The index of the component within the pattern. */
      var index: ComponentIndex = 0

      /**
       * Return the number of matchings of this pattern component in
       * the target mixture
       */
      // FIXME: implement!
      def count: Int = 1

      /** Update the component pointers in the agents. */
      private def registerAgents {
        var i = 0
        for (a <- agents) {
          a.component = this
          a.index = i
          i += 1
        }
      }

      registerAgents
    }

    /**
     * Factory method creating an empty pattern.
     *
     * @return an empty pattern.
     */
    def apply() = new Pattern(Vector(), Vector(), "")

    // def apply(agent: Agent) = new Pattern(Vector(
    //   new Component(Vector(agent))), Vector((0, 0)))

    /**
     * Factory method creating a pattern from a string.
     *
     * This factory method invokes the [[Parser]] to parse a kappa
     * expression.  It then walks the [[Parser.AST]] and builds a
     * [[Patterns.Pattern]] from the expression.
     *
     * @return a pattern corresponding to the expression `expr`.
     */
    // TODO: Fix this. The problem right now is that the way states
    // are handled is inconsistent.  States should point to the symbol
    // table (environment), not the other way around.
    
    // def apply(expr: String) = {
    //   val ast = parseSiteGraph(expr) match {
    //     case Success(ast, _) => ast
    //     case msg => println(msg); println();
    //     throw new IllegalArgumentException(
    //       "given site graph '" + expr + "' is invalid")
    //   }

    //   val lstates = for (AST.LinkAnnot(label, lstate) <- ast)
    //                 yield (label, lstate)
    //   val lstateMap = Map(lstates: _*).lift

    //   val links = for {
    //     (AST.Agent(aname, _, intf), id) <- ast.zipWithIndex
    //     AST.Site(sname, _, AST.Linked(label)) <- intf
    //   } yield (label, id, env.sitenameId(aname)(sname))

    // val pairs = (links groupBy (_._1) values) map {
    //   case (label, a1, s1) :: (_, a2, s2) :: Nil => (a1, s1, a2, s2, label)
    //   case _ => throw new IllegalArgumentException("every bond label must appear exactly twice")
    // }

    // val agents : Seq[Agent] =
    //   for ((AST.Agent(aname, astate, intf), id) <- ast.zipWithIndex)
    //   yield {
    //     val anameId = env.agentnameId(aname)
    //     val sites = for (AST.Site(sname, sstate, lnk) <- intf)
    //                 yield new Site(env.sitenameId(aname)(sname), sstate map env.sitestateId(aname)(sname), lnk match {
    //                   case AST.Stub          => Stub
    //                   case AST.Undefined     => Undefined
    //                   case AST.Wildcard      => Wildcard(None)
    //                   case AST.Linked(label) => Undefined // Linked
    //                 })
    //     val siteMap : Map[SiteId, Site] = Map(sites map (site => (site.name, site)):_*) withDefault (sId => Site(sId, None, Undefined))
    //     val interface : Seq[Site] = 0 until env.sitename(anameId).length map siteMap
    //     val agent = new Agent(anameId, astate map env.agentstateId(aname), interface.to[Vector], id)
    //     agent foreach (_.agent = Some(agent))
    //     agent
    //   }

    // // Connect sites
    // pairs foreach {
    //   case (a1, s1, a2, s2, label) => val lstate = lstateMap(label) map (env.linkstateId((agents(a1).name, s1, agents(a2).name, s2))(_))
    //                                   agents(a1)(s1).link = Linked(agents(a2)(s2), lstate) ;
    //                                   agents(a2)(s2).link = Linked(agents(a1)(s1), lstate)
    // }

    // val agentIds : Set[AgentId] = agents.indices.to[Set]

    // // Compute connected components
    // def traverse(agent: Agent, queue: Seq[Agent], visited: Set[AgentId], ccId: ComponentId, ccMap: CcMap) : CcMap = {
    //   def next(queue: Seq[Agent]) = {
    //     val vis = visited + agent.id
    //     queue match {
    //       case next :: tl => traverse(next, tl, vis, ccId, ccMap updated (agent.id, ccId))
    //       case Nil => (agentIds -- vis) headOption match {
    //         case Some(nextId) => traverse(agents(nextId), List(), vis, ccId + 1, ccMap updated (agent.id, ccId))
    //         case None => ccMap updated (agent.id, ccId)
    //       }
    //     }
    //   }
    //   if (visited contains agent.id)
    //     next(queue)
    //   else {
    //     val nbs = for (Site(_, _, Linked(nb, _)) <- agent) yield nb.agent.get
    //     next(queue ++ nbs)
    //   }
    // }

    // val ccMap = agents match {
    //   case a1 :: _ => traverse(a1, List(), Set(), 0, Vector.fill(agents.length)(-1))
    //   case Nil     => Vector()
    // }

    // val p = new Pattern(ccMap.max, ccMap, agents.to[Vector], siteGraph)
    // p foreach (_.pattern = Some(p))
    // p

  }
}
