package kappa

import scala.language.implicitConversions

import scala.collection.mutable
import scala.collection.LinearSeq


trait Mixtures {
  this: LanguageContext
      with SiteGraphs
      with ContactGraphs
      with Patterns
      with Embeddings
      with RollbackMachines =>

  import SiteGraph.{Link, Stub, LinkType, AgentType, SiteType}


  /**
   * A class representing mixtures in [[kappa.Model]]s (i.e. site
   * graphs).
   *
   * This class is at the heart of the simulator and hence needs to be
   * optimized for simulation operations.  As a result the design and
   * implementation of this class follows a very "mutable" style and
   * sacrifice ease of use and code readability for performance to a
   * certain degree.  Not much effort is made to guarantee the
   * consistency of mixtures overall.  Hence, mixtures should be
   * handled with care and, if possible, only interacted with through
   * actions and embeddings.  Some of the design choices are outlined
   * and explained in more detail below.
   *
   * There are two operations in particular that need to be very
   * efficient during simulation:
   *
   *  1. extending pairs of pattern component/mixture agents into
   *     partial embeddings,
   *
   *  2. applying actions to mixtures.
   *
   * To extend partial embeddings efficiently, it is important to be
   * able to traverse mixtures efficiently (along the links in the
   * underlying site graph).  However, the way we organize agents as a
   * "collection" in the mixture (e.g. the order in which agents are
   * stored) is largely irrelevant for efficient extension.
   *
   * To apply actions efficiently, it is important that we are able to
   * quickly ''add and delete'' agents to mixtures.  Deleting agents
   * in e.g. an Array or a Vector is a no-go as this takes O(n) time.
   * Instead, we need a data structure where these operations are
   * cheap.  Two data structures that fulfill this criterion are
   *
   *  a. doubly-linked lists, and
   *
   *  b. mutable sets/maps.
   *
   * The [[scala.collection.mutable.HashSet]] class from the Scala
   * collections library would be a good choice for convenience:
   * accessing/adding/deleting an element is O(log n).  But compared
   * to a simple doubly-linked list, hash sets are still rather slow,
   * and the fact that we have to make sure we don't accidentally add
   * "duplicate" agents, etc. just increase complexity unnecessarily.
   * The collections library also provides a mutable doubly-linked
   * list, [[scala.collection.mutable.DoubleLinkedList]], that we
   * considered using for the implementation of this class.  However,
   * this would still require some overhead, e.g. in order to delete a
   * given agent, that agent needs to maintain a reference (a
   * back-pointer) to the "cons cell" (i.e. the instance of
   * `DoubleLinkedList[Agent]`) that contains it, or alternatively one
   * would need to pass around "cons cells" instead of agents for
   * quick access.  In the end we decided to just implement our own
   * doubly-linked list, which seemed acceptable for a data structure
   * that is supposed to be optimized for speed.
   *
   * As a result, mixtures are mutable and organized as doubly linked
   * lists of [[Mixture.Agent]]s.  Operations on mixtures are
   * destructive in the sense that the mixture is modified in place
   * and if it appears "on the left hand side" (e.g. as an argument to
   * the [[++=]] method) it might be destroyed in the process.  No
   * attempt is made to guarantee the consistency of such a zombie
   * mixture, and indeed not much effort is made to guarantee the
   * consistency of mixtures overall.  Again, in general, mixtures
   * should be handled with care and, if possible, only through
   * actions and embeddings.
   *
   * @constructor create an empty Mixture.
   */
  // Finally, there are two more advantages of this design over
  // using a standard class from the collections library:
  //
  //  1) This class is close to the "heap" data structures used in
  //     KaSim.  A KaSim "heap" is what is otherwise often called a
  //     "memory pool" data structure (I think the term "heap"
  //     refers to the concept as used in runtime environments
  //     rather than the "heap" data-structure from algorithmics).
  //     The point of a memory pool is to manage memory
  //     explicitly. In the KaSim case it's used to avoid garbage
  //     collection of agents: instead of being collected by the GC,
  //     the agent is marked "free" so that the memory allocated for
  //     it can be reused later.  See
  //     [[http://en.wikipedia.org/wiki/Memory_pool]] for more
  //     information.
  //
  //     If we decide to use a memory pool later on, we might
  //     consider pre-allocating agents in an array, and use indices
  //     instead of pointers for next, prev, etc.  See
  //     [[http://en.wikipedia.org/wiki/Linked_list#Linked_lists_using_arrays_of_nodes]]
  //     for the benefits of this approach.
  //
  //  2) This class could easily be ported to e.g. C/C++ in case we
  //     want to target other target languages in an LMS-based
  //     implementation.
  final class Mixture
      extends LinearSeq[Mixture.Agent]
         with DoublyLinkedList[Mixture.Agent, Mixture]
         with RollbackMachine
         with Marks {

    import Mixture.{Agent, AgentImpl, Linked}

    /** The [[Markable]] instances in this mixture are [[Agent]]s. */
    type T = Mixture.Agent


    override def newBuilder = new MixtureBuilder

    class MixtureBuilder extends mutable.Builder[Agent, Mixture] {

      val m = new Mixture

      def +=(u: Agent): this.type = {
        // Make a disconnected copy of `u` and add it to `m`
        val st = u.siteTypes
        val ss = u.siteStates.clone
        val ls = new Array[Link](u.length)
        // TODO: Is the agent state shared with the copy?
        val v = new AgentImpl(u.agentType, u.state, st, ss, ls)
        v._copy = u
        v._mixture = m
        m += v
        this
      }

      def result: Mixture = {

        // Now setup the interfaces of the agents in `that`
        for (v <- m) {
          val u = v._copy
          for (i <- u.indices) {
            v.links(i) = u.links(i) match {
              case Linked(a, j, t, l) => Linked(a._copy, j, t, l)
              case Stub => Stub
              case _ => throw new IllegalStateException(
                "encountered mixture with an undefined or wildcard link")
            }
          }
        }

        // Reset `copy` references in the agents of `this` to avoid
        // memory leaks.
        for (v <- m)
          v._copy = null

        m
      }

      def clear { m.clear }
    }


    /** Add a single agent to this mixture. */
    override def +=(agent: Agent): this.type = {

      agent._mixture = this

      // Clear all marks from agent because it comes from
      // another mixture where it can have been marked
      unmark(agent)

      super.+=(agent)
      this
    }

    /** Create a [[Mixture.Agent]] and add it to this mixture. */
    def +=(agentType: AgentType, state: AgentState,
      siteTypes: Seq[SiteType], siteStates: Seq[SiteState])
        : this.type = {

      // Create and initialize sites
      val st = siteTypes.toArray
      val ss = siteStates.toArray
      val ls = Array.fill[Link](ss.length)(Stub)

      // Add agent to mixture
      this += new AgentImpl(agentType, state, st, ss, ls)
    }

    /**
     * Append another mixture to this one.
     *
     * The doubly-linked list representing the mixture `that` will be
     * appended to this mixture.  After this concatenation `that`
     * becomes invalid and should not be operated on any longer.
     *
     * @param that the mixture to append to `this`.
     * @return this mixture with `that` appended to it.
     */
    def ++=(that: Mixture): this.type = {

      for (a <- that) {
        a._mixture = this
        unmark(a)
      }

      super.++=(that)
    }

    /** Remove a single agent from this mixture. */
    override def -=(agent: Agent): this.type = {

      // Disconnect agent
      for (Linked(u, i, _, _) <- agent.links)
        u.links(i) = Stub

      super.-=(agent)
    }

    // RHZ These two methods should belong to Site
    // NB connect should return the link and the link should have a method
    // withState for cooler syntax
    //
    // sstucki: Not necessarily. The reason they are here instead is
    // that I would like for every atomic action to be a method on a
    // mixture.  Or in other words, for every atomic action, there
    // should be a method in this class that has the same signature as
    // that action.  In that way, any (non-atomic) action is also a
    // function/method on a mixture.  To be precise, actions are
    // defined to act on a mixture ''and'' a set of agents.  In a
    // sense they are functions on mixtures (i.e. for agent/subgraph
    // addition and deletion) and agents (i.e. for state changes and
    // link addition and deletion).  The actual instances of both the
    // mixture and the specific agents they will act on are
    // only defined w.r.t. an embedding.  In that sense the embedding
    // is a sequence of actual agent parameters to the action, an
    // environment that binds the formal agent parameter to actual
    // ones.
    //
    // It is for the same reason that the signature these two methods
    // looks as it does, i.e. taking a pair of agents and site indices
    // rather than a pair of sites.  In the action the agent is a
    // formal parameter, and hence we can't possibly know about the
    // actual sites, only about their index with respect to the formal
    // agent parameter.
    //
    // Now granted, the way Agents are set up in this class, it's
    // not necessary to refer to a mixture in order to establish a
    // connection and we can always find the site for a given
    // agent-site index pair.  So we might as well implement these
    // methods in Agent rather than in Mixture, or indeed in Site
    // (given that we add a back pointer from sites to Agents, which
    // has other drawbacks, see comment/documentation on Linked).  In
    // both cases we could then have some extra methods in Mixture
    // that just forward the call to the appropriate Sites.  But then
    // we might as well do the opposite and just forward the calls
    // from the sites/agents to the mixtures.
    //
    // About the cooler syntax: the way you proposed won't work if
    // Links are immutable.  But a connect method in Agent/Site could
    // return the Site which would then have a "withState" method that
    // would create a new Linked instance with the updated link state.
    // I think Mixture is not the place for this because I still think
    // that this class is not supposed to be manipulated directly in
    // this way.  Then again, as long as it doesn't hurt performance,
    // we might always add it just for fun.
    //
    // RHZ: I think it's important to be able to manipulate Mixtures
    // easily to let the user define perturbations that modify the
    // Mixture or stop the simulation to run a deterministic sequence
    // of actions without having to create embeddings manually for
    // that.  See for instance DoubleStrandBreakModel.scala.

    /** Connect two sites in this mixture. */
    def connect(
      u1: Agent, i1: SiteIndex, t1: LinkType, l1: LinkState,
      u2: Agent, i2: SiteIndex, t2: LinkType, l2: LinkState)
        : Mixture = {

      val freshId = Mixture.getFreshLinkId
      u1.links(i1) = Linked(u2, i2, t1, l1 withLinkId freshId)
      u2.links(i2) = Linked(u1, i1, t2, l2 withLinkId freshId)
      this
    }

    /** Disconnect a site in this mixture. */
    def disconnect(u: Agent, i: SiteIndex): Mixture = {

      u.links(i) match {
        case Linked(u2, i2, _, l1) => {
          u2.links(i2) = Stub
          Mixture.recycleLinkId(l1.id)
        }
        case _ => throw new IllegalStateException(
          "trying to disconnect a non-connected agent")
      }
      u.links(i) = Stub
      this
    }

    /**
     * Update the state of an agent in this mixture.
     *
     * @param u the agent to modify.
     * @param state the new state of `agent`.
     * @return this mixture with the state of `agent` changed to
     *         `state`.
     */
    // def updateAgentState(u: Agent, s: AgentState): Mixture = {
    //   checkpointAgent(u)
    //   u.state = s
    //   mark(u, Updated)
    //   this
    // }

    /**
     * Update the state of a site of some agent in this mixture.
     *
     * @param u the agent containing the site to modify.
     * @param i the index of the site to modify.
     * @param s the new state of the site to modify.
     * @return this mixture with the state of the site at `siteIdx`
     *         changed to `state`.
     */
    // def updateSiteState(u: Agent, i: SiteIndex, s: SiteState): Mixture = {
    //   checkpointAgent(u)
    //   u._siteStates(i) = s
    //   mark(u, Updated)
    //   this
    // }

    // -- Traversals --

    /** Traverse the mixture by jumping from one agent `u` to its
      * neighbour at site `site`(u) and call `f` on each agent that
      * finds on the way.
      */
    def traverse(u: Agent, site: Agent => SiteIndex, f: Agent => Unit) {
      def trav(agentOpt: Option[Agent]) {
        agentOpt match {
          case Some(u) => { f(u); trav(u.neighbour(site(u))) }
          case None => ()
        }
      }
      trav(Some(u))
    }

    def traverse(u: Agent, i: SiteIndex)(f: Agent => Unit): Unit =
      traverse(u, _ => i, f)

    // def traverse(u: Agent, s: SiteState)(f: Agent => Unit): Unit =
    //   traverse(u, u => u.siteIndex(s), f)


    override def toString = iterator.mkString("", ",", "")

    def toPattern: Pattern = {

      // Create a builder to build this pattern
      val pb = new Pattern.Builder()

      // Create a map to track which site a given link connects to.
      val linkMap = new mutable.HashMap[(Agent, SiteIndex),
        (pb.Agent#Site, LinkType, LinkState)]()

      for (u <- this) {
        val v = pb += (u.agentType, u.state)
        for (s <- u.sites) v += (s.siteType, s.state)

        for ((s, i) <- u.sites.zipWithIndex) {
          val x = v.sites(i)
          s.link match {
            case Stub => x define SiteGraph.Stub
            case Linked(v, j, t, l) =>
              linkMap get (u, i) match {
                case Some((y, u, m)) => x connect (y, t, l, u, m)
                case None => linkMap += ((v, j) -> (x, t, l))
              }
            case _ => ()
          }
        }
      }

      // Build the pattern
      pb.build
    }
  }

  object Mixture extends SiteGraph {

    /** The type of agents in [[Mixture]]s. */
    type Agent = AgentImpl

    // -- Manage LinkIds in mix --

    // NOTE: This is a little hack. Even when a more elegant solution
    // is desirable, it's not clear that one can be easily achieved
    // while making toString work nicely.

    // FIXME: link id reclycing doesn't seem to be working properly
    val recycledIds: mutable.Queue[LinkId] = mutable.Queue()
    var freshId: LinkId = -1

    def getFreshLinkId: LinkId =
      if (recycledIds.isEmpty) {
        freshId += 1
        freshId - 1
      } else {
        recycledIds.dequeue
      }

    def recycleLinkId(id: LinkId) {
      recycledIds.enqueue(id)
    }

    def initialiseLinkIds {
      var freshId: LinkId = 0
      val visited: mutable.Set[(Agent, SiteIndex)] = mutable.Set()
      for {
        u <- mix
        (Linked(v, j, t1, l1), i) <- u.links.zipWithIndex
      } {
        if (!(visited contains (v, j))) {
          val (t2, l2) = v.links(j) match {
            case Linked(_, _, t2, l2) => (t2, l2)
            case _ => throw new IllegalStateException(
              "dangling link in mixture")
          }
          u.links(i) = Linked(v, j, t1, l1 withLinkId freshId)
          v.links(j) = Linked(u, i, t2, l2 withLinkId freshId)
          freshId += 1
          visited += (u -> i)
        }
      }
      this.freshId = freshId
    }


    /**
     * A class representing agents in [[Mixture]]s.
     *
     * This class has two roles: it acts as a container for agent
     * information (agent state, interfaces, lift maps) and, at the
     * same time, as a cell in a doubly linked list that forms the
     * mixture.  The reason for this double-role is that in this way
     * an [[Agent]]s can be pointed to directly (i.e. without an extra
     * layer of indirection, e.g. in
     * [[ComponentEmbeddings#ComponentEmbedding]]s) and removed from
     * the mixture efficiently (i.e. without the need of a
     * back-pointer from an agent to its list cell).
     *
     * TODO: Should we make this an inner class of [[Mixture]]?
     *
     * @param state the state of this agent.
     * @param sites the interface of this agent.
     * @param liftMap lift map for component embeddings.
     * @param mixture a reference to the [[Mixture]] this agent belongs to.
     * @param next a reference to the next agent in the [[Mixture]]
     *        this agent belongs to.
     * @param prev a reference to the previous agent in the [[Mixture]]
     *        this agent belongs to.
     */
    final class AgentImpl(
      val agentType: AgentType,
      var state: AgentState,
      val siteTypes: Array[SiteType],
      // NOTE: `siteStates` and `links` need to be set when
      // rollbacking a mixture (RollbackMachines.scala, line 78)
      val siteStates: Array[SiteState],
      // NOTE: `links` need to be set when creating a Mixture
      // from a Pattern (Patterns.scala, line 105)
      val links: Array[Link],
      val liftMap: mutable.HashMap[Pattern.Agent,
        ComponentEmbedding[Agent]] = new mutable.HashMap())

        extends AgentIntf
           with DoublyLinkedCell[Agent]
           with RollbackAgent
           with Markable {

      // TODO: Change all access modifier to protected[Mixture]?

      /** A reference to a copy of this agent. */
      protected[kappa] var _copy: Agent = null

      /** A reference to the mixture this agent belongs to. */
      protected[kappa] var _mixture: Mixture = null

      /** The mixture this agent belongs to. */
      @inline def mixture =
        if (_mixture == null) throw new NullPointerException(
          "attempt to access parent mixture of orphan agent")
        else _mixture

      /**
       * Register a component embedding in the lift map of this agent.
       *
       * @param u the pre-image of this agent in the component embedding.
       * @param ce the component embedding to add.
       */
      def addLift(u: Pattern.Agent, ce: ComponentEmbedding[Agent]) {
        liftMap += ((u, ce))
      }

      /**
       * Remove a component embedding from the lift map of this agent.
       *
       * @param u the pre-image of this agent in the component embedding.
       * @param ce the component embedding to remove.
       */
      def removeLift(u: Pattern.Agent) {
        liftMap -= u
      }

      /**
       * Check all component embeddings in the lift set for
       * consistency and remove those that are no longer valid.
       */
      def pruneLifts(checkpoint: Boolean) {
        val invalidLifts =
          if (this hasMark Deleted) liftMap
          else liftMap filterNot { case (u, ce) =>
            (u matches this) && (u.indices forall {
            j => (u.neighbour(j), this.neighbour(j)) match {
              case (None, _) => true
              case (Some(w1), Some(w2)) => ce(w1.index) == w2
              case _ => false
            }
          })
        }
        for ((u, ce) <- invalidLifts) {
          if (checkpoint)
            mix.checkpointRemovedEmbedding(ce)
          // removeEmbeddings takes care of removing the lifts
          ce.component.removeEmbedding(ce)
        }
      }


      // -- Mixture.AgentIntf API --

      // /** The type of the sites of this agent. */
      // @inline def siteTypes: IndexedSeq[agentType.Site] = _siteTypes

      // /** The states of the sites of this agent. */
      // @inline def siteStates: IndexedSeq[SiteState] = _siteStates

      // /** The links of the sites of this agent. */
      // @inline def links: IndexedSeq[Link] = _links

      /** The number of sites of this agent. */
      @inline override def length: Int = links.length

      /** The range of indices of this agent. */
      @inline override def indices: Range = links.indices
    }
  }

  /** Default Mixture of the enclosing model. */
  val mix = new Mixture
}

