package kappa

import scala.language.postfixOps

trait Patterns extends LanguageContext with Parser with Environment {
  self =>

  // Links
  sealed abstract class Link
  case object Undefined extends Link
  case object Stub extends Link
  case class Linked(to: Site, state: Option[LinkStateId]) extends Link
  case class Wildcard(state: Option[LinkStateId]) extends Link

  // Sites
  case class Site(name: SiteId, state: Option[SiteStateId], var link: Link) { // link needs to be mutable =(
    var agent : Option[Agent] = None

    def neighbour : Option[Site] = link match {
      case Linked(to, _) => Some(to)
      case _ => None
    }

    override def toString() =
      env.sitename(agent.get.name)(this.name) +
      (this.state match {
        case Some(sstateId) => ":" + env.sitestate(agent.get.name)(this.name)(sstateId)
        case None => ""
      }) +
      (this.link match {
        case Undefined => "?"
        case Wildcard(_) => "!_"
        case Stub => ""
        case Linked(to, _) => "!" + env.agentname(to.agent.get.name) + "." + env.sitename(to.agent.get.name)(to.name)
      })
  }

  // Agents
  case class Agent(name: AgentNameId, state: Option[AgentStateId], intf: Vector[Site], id: AgentId) extends Seq[Site] {
    var pattern : Option[Pattern] = None

    override def toString() = env.agentname(this.name) + "(" + intf.map(_.toString).mkString(", ") + ")"

    // Seq methods
    def apply(idx: Int) : Site = intf(idx)
    def iterator : Iterator[Site] = intf.iterator
    def length : Int = intf.length
  }

  type ComponentId = Int
  type CcMap = Vector[ComponentId]

  // Site graphs
  class Pattern(val arity: Int, val ccId: CcMap, protected val agents: Vector[Agent], _siteGraphString: String) extends Seq[Agent] {
    // Return the number of matchings of this in the target reaction mixture
    def inMix() = 1
    // Q: should we use the square approximation here? or should we have
    //    a special class for site graphs that are connected components and
    //    only define count in that class?

    // Concatenate or compose two site graphs
    def ++(that: Pattern) = {
      val p = new Pattern(this.arity + that.arity,
                          this.ccId ++ (that.ccId map (_ + this.arity)),
                          this.agents ++ (that.agents map {
                            case Agent(name, state, intf, id) => new Agent(name, state, intf, id + this.arity) }),
                          this.toString + ", " + that.toString)
      p foreach (_.pattern = Some(p))
    }

    override def toString() = _siteGraphString

    // Action constructor
    def -> (rhs: Pattern) = new Action(this, rhs)

    // Seq methods
    def apply(idx: Int) : Agent = agents(idx)
    def iterator : Iterator[Agent] = agents.iterator
    def length : Int = agents.length
  }

  def createPattern(siteGraph: String) = {
    val sg = parseSiteGraph(siteGraph) match {
      case Success(sg, _) => sg
      case msg => println(msg); println();
                  throw new IllegalArgumentException("given site graph '" + siteGraph + "' is invalid")
    }
    val lstates = for (AST.LinkAnnot(label, lstate) <- sg) yield (label, lstate)
    val lstateMap : Int => Option[LinkState] = Map(lstates:_*).lift

    val links = for ((AST.Agent(aname, _, intf), id) <- sg.zipWithIndex; AST.Site(sname, _, AST.Linked(label)) <- intf)
                yield (label, id, env.sitenameId(aname)(sname))

    val pairs = (links groupBy (_._1) values) map {
      case (label, a1, s1) :: (_, a2, s2) :: Nil => (a1, s1, a2, s2, label)
      case _ => throw new IllegalArgumentException("every bond label must appear exactly twice")
    }

    val agents : Seq[Agent] =
      for ((AST.Agent(aname, astate, intf), id) <- sg.zipWithIndex)
      yield {
        val anameId = env.agentnameId(aname)
        val sites = for (AST.Site(sname, sstate, lnk) <- intf)
                    yield new Site(env.sitenameId(aname)(sname), sstate map env.sitestateId(aname)(sname), lnk match {
                      case AST.Stub          => Stub
                      case AST.Undefined     => Undefined
                      case AST.Wildcard      => Wildcard(None)
                      case AST.Linked(label) => Undefined // Linked
                    })
        val siteMap : Map[SiteId, Site] = Map(sites map (site => (site.name, site)):_*) withDefault (sId => Site(sId, None, Undefined))
        val interface : Seq[Site] = 0 until env.sitename(anameId).length map siteMap
        val agent = new Agent(anameId, astate map env.agentstateId(aname), interface.to[Vector], id)
        agent foreach (_.agent = Some(agent))
        agent
      }

    // Connect sites
    pairs foreach {
      case (a1, s1, a2, s2, label) => val lstate = lstateMap(label) map (env.linkstateId((agents(a1).name, s1, agents(a2).name, s2))(_))
                                      agents(a1)(s1).link = Linked(agents(a2)(s2), lstate) ;
                                      agents(a2)(s2).link = Linked(agents(a1)(s1), lstate)
    }

    val agentIds : Set[AgentId] = agents.indices.to[Set]

    // Compute connected components
    def traverse(agent: Agent, queue: Seq[Agent], visited: Set[AgentId], ccId: ComponentId, ccMap: CcMap) : CcMap = {
      def next(queue: Seq[Agent]) = {
        val vis = visited + agent.id
        queue match {
          case next :: tl => traverse(next, tl, vis, ccId, ccMap updated (agent.id, ccId))
          case Nil => (agentIds -- vis) headOption match {
            case Some(nextId) => traverse(agents(nextId), List(), vis, ccId + 1, ccMap updated (agent.id, ccId))
            case None => ccMap updated (agent.id, ccId)
          }
        }
      }
      if (visited contains agent.id)
        next(queue)
      else {
        val nbs = for (Site(_, _, Linked(nb, _)) <- agent) yield nb.agent.get
        next(queue ++ nbs)
      }
    }

    val ccMap = agents match {
      case a1 :: _ => traverse(a1, List(), Set(), 0, Vector.fill(agents.length)(-1))
      case Nil     => Vector()
    }

    val p = new Pattern(ccMap.max, ccMap, agents.to[Vector], siteGraph)
    p foreach (_.pattern = Some(p))
    p
  }


  // TODO the following code should be in another trait/file,
  // but the -> method in Pattern requires Action to be in scope
  var rules : Vector[Rule] = Vector()

  // Actions
  class Action(lhs: Pattern, rhs: Pattern) {
    def :@ (rate: => Double) = new Rule(lhs, rhs, ns => ns.product * rate)
    def !@ (law: (Int*) => Double) = new Rule(lhs, rhs, law) // see https:/ / github.com/jkrivine/KaSim/issues/9
  }

  // Rules
  class Rule(lhs: Pattern, rhs: Pattern, rate: (Int*) => Double) {
    lazy val ones : Stream[Int] = Stream.cons(1, ones);
    override def toString() = lhs.toString + " -> " + rhs.toString + " :@ " + rate(ones.take(lhs.arity):_*);

    // Add every instance of this class to the rules vector
    self.rules = self.rules :+ this;
  }
}
