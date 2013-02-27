package kappa

import scala.collection.mutable

import scala.language.implicitConversions

trait Mixtures {
  mixtures: LanguageContext with Patterns with Embeddings =>

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
  final class Mixture extends Seq[Mixture.Agent] {
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

    import Mixture._

    /** First agent in the doubly-linked list representing the mixture. */
    private var _head: Agent = null

    /** First agent in the doubly-linked list representing the mixture. */
    override def head: Agent =
      if (_head == null) throw new NoSuchElementException(
        "attempt to reference head element in empty mixture")
      else _head

    /** The number of agents in this mixture. */
    private var _length: Int = 0

    private class MixtureIterator extends Iterator[Agent] {
      private var nextAgent: Agent = _head

      def next = {
        val n = nextAgent
        if (n != null) nextAgent = n.next
        n
      }

      def hasNext = nextAgent != null
    }

    /** The collection used to track marked agents. */
    private var _markedAgents: List[Agent] = Nil

    /**
     * The collection used to track marked agents.
     *
     * NOTE: A call to this method will prune the marked agents list
     * before returning it, which may take O(n) time.  However, if the
     * list is subsequently iterated over, the total amortized access
     * time of the call to this method is just O(1) as the pruning
     * time is proportional to previous calls to [[Mixture.unmark]]
     * plus the number of subsequent iteration steps over the pruned
     * list.
     *
     * FIXME: We could do better.  We could prune the list lazily,
     * thereby fixing the amortized cost to O(1) even for cases where
     * the list is not iterated over completely.
     *
     * @return the collection used to track marked agents.
     */
    def markedAgents = {
      _markedAgents = _markedAgents filter (_._marked == true)
      _markedAgents
    }

    /**
     * Mark a given agent and add it to the marked agents list (unless
     * it was already marked).
     */
    @inline def mark(agent: Agent) {
      if (!agent._marked) {
        agent._marked = true
        _markedAgents = agent :: _markedAgents
      }
    }

    /** Unmark a given agent and add it to the marked agents list. */
    @inline def unmark(agent: Agent) {
      agent._marked = false
    }

    /**
     * Unmark all the agents in the marked agents list and clear the
     * list.
     */
    @inline def clearMarkedAgents {
      for (agent <- _markedAgents) agent._marked = false
      _markedAgents = Nil
    }


    /**
     * A class representing a single mixture checkpoint.
     *
     * FIXME: Should we use [[Mixture]] for this?
     */
    final class Checkpoint(
      val head: Agent, val length: Int, var agents: List[Agent])

    /** The stack of mixture checkpoints. */
    private var _checkpoints: List[Checkpoint] = Nil

    /**
     * Make a checkpoint of this mixture.
     *
     * After a call to this method, every update to an agent of this
     * mixture through the any of the [[Mixture]]'s methods will cause
     * the corresponding agent to be backed up.  The mixture may be
     * reset to its state prior to the call to this method by calling
     * [[Mixture.rollback]].
     *
     * Mixture checkpoints are stored on a stack, i.e. in FIFO order.
     * Hence one may chose to call this method multiple times before
     * calling [[Mixture.rollback]].  Each such call will result in a
     * new checkpoint.
     *
     * NOTE: this method will not copy the agent, site and link state
     * types (represented by [[LanguageContext#AgentState]],
     * [[LanguageContext#SiteState]] and
     * [[LanguageContext#LinkState]]) when creating a checkpoint.
     * Instead, the checkpoint will just contain references to such
     * classes.  If these classes contain mutable state, updates to
     * that state will be reflected in the checkpoint and can not be
     * reverted through a call to [[Mixture.rollback]].
     *
     * NOTE 2: The lift sets of an agents are not checkpointed.
     *
     * @return this mixture.
     */
    def checkpoint: Mixture = {
      _checkpoints = (new Checkpoint(_head, _length, Nil)) :: _checkpoints
      this
    }

    /**
     * Restore the state of this mixture to the last checkpoint.
     *
     * See [[Mixture.checkpoint]] for details.
     *
     * @return this mixture.
     */
    def rollback: Mixture = {
      if (_checkpoints.isEmpty) throw new IllegalStateException(
        "attempt to roll back mixture with empty checkpoint stack")

      // After the rollback, the marked agents list should contain
      // exactly those agents that were rolled back.  Start by
      // resetting it.
      clearMarkedAgents

      // Roll back all the agents in the checkpoint.
      val cp = _checkpoints.head
      for (v <- cp.agents) {
        val u = v.copy

        // Restore the state of `u` to that of `v`.
        u.state = v.state
        for (i <- v.sites.indices) {
          u.sites(i).state = v.sites(i).state
          u.sites(i).link  = v.sites(i).link
        }
        u.prev = v.prev
        u.next = v.next
        u.prev.next = u
        u.next.prev = u
        mark(u)
      }

      // Restore the head and length fields of the mixture.
      _head = cp.head
      _length = cp.length

      // Remove the checkpoint from the top of the stack.
      _checkpoints = _checkpoints.tail
      this
    }

    /**
     * Discard the latest checkpoint of this mixture.
     *
     * See [[Mixture.checkpoint]] for details.
     *
     * @return this mixture with its last checkpoint discarded.
     */
    def discardCheckpoint: Mixture = {
      _checkpoints = _checkpoints match {
        case _ :: cps => cps
        case Nil => throw new IllegalStateException(
          "attempt to discard checkpoint of mixture with empty " +
            "checkpoint stack")
      }
      this
    }

    /**
     * Make a copy of this agent and add it to the current checkpoint.
     *
     * If the checkpoint stack is empty or if the agent is marked, no
     * copy will be created.  If a copy is created, its `copy` field
     * will point to the original agent `u`.
     *
     * FIXME: Currently, the mark/unmark mechanism is used to decide
     * which agents should be checkpointed.  We should probably have a
     * separate flag for tracking checkpointed agents.
     *
     * @param u the agent to add to the current checkpoint.
     */
    @inline private def checkpointAgent(u: Agent) {
      if (!_checkpoints.isEmpty && !u.marked) {
        val v = u.checkpointCopy
        val cp = _checkpoints.head
        cp.agents = v :: cp.agents
      }
    }

    /**
     * Make a copy of this mixture.
     *
     * The resulting mixture is completely independent of the original
     * and can hence be operated on without the fear of destroying the
     * original.
     *
     * @return a copy of this mixture.
     */
    def copy: Mixture = {
      var u: Agent = _head
      val that = new Mixture
      that._length = this._length

      // First allocate "empty" agents for `that`
      var p: Agent = null
      while(u != null) {
        val sites = new Array[Site](u.sites.size)
        val v = new Agent(u.state, sites)
        v.mixture = that
        v.prev = p
        if (p == null) { that._head = v } else { p.next = v }
        u.copy = v
        p = v
        u = u.next
      }

      // Now setup the interfaces of the agents in `that`
      u = _head
      while(u != null) {
        val v = u.copy
        for (i <- 0 until u.length) {
          val s = u(i)
          val l = s.link match {
            case Linked(a, j, l) => Linked(a.copy, j, l)
            case Stub => Stub
          }
          v.sites(i) = Site(s.state, l)
        }
        u = u.next
      }

      // Reset `copy` references in the agents of `this` to avoid
      // memory leaks.
      u = _head
      while(u != null) {
        u.copy = null
        u = u.next
      }
      that
    }

    /** Add a single agent to this mixture. */
    def +=(agent: Agent): Mixture = {
      agent.mixture = this
      agent.prev = null
      agent.next = _head
      if (_head != null) _head.prev = agent
      _head = agent
      _length += 1

      unmark(agent)
      mark(agent)

      this
    }

    /** Create a [[Mixture.Agent]] and add it to this mixture. */
    def +=(state: AgentState, siteStates: Seq[SiteState]): Mixture = {

      // Create and initialize sites
      var i: Int = 0
      val sites = new Array[Site](siteStates.size)
      for (s <- siteStates) {
        sites(i) = new Site(s)
        i += 1
      }

      // Add agent to mixture
      this += (new Agent(state, sites))
    }

    /** Remove a single agent from this mixture. */
    def -=(agent: Agent): Mixture = {

      checkpointAgent(agent)

      // Disconnect agent
      for (s <- agent) {
        s.link match {
          case Linked(u, i, _) => {
            checkpointAgent(u)
            u.sites(i).link = Stub
            mark(u)
          }
          case _ => { }
        }
      }

      // Remove agent
      val ap = agent.prev
      val an = agent.next
      if (ap != null) ap.next = an
      if (an != null) an.prev = ap
      if (_head == agent) _head = an
      _length -= 1
      this
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
    // is sequence of actual agent parameters to the action, an
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

    /** Connect two sites in this mixture. */
    def connect(u1: Agent, s1: SiteIndex, l1: LinkState,
                u2: Agent, s2: SiteIndex, l2: LinkState): Mixture = {

      checkpointAgent(u1)
      checkpointAgent(u2)

      u1.sites(s1).link = Linked(u2, s2, l1)
      u2.sites(s2).link = Linked(u1, s1, l2)

      mark(u1)
      mark(u2)

      this
    }

    /** Disconnect a site in this mixture. */
    def disconnect(u: Agent, s: SiteIndex): Mixture = {
      val s1 = u.sites(s)
      s1.link match {
        case Linked(u2, i, _) => {
          checkpointAgent(u2)
          u2.sites(i).link = Stub
          mark(u2)
        }
        case _ => { }
      }
      checkpointAgent(u)
      s1.link = Stub
      mark(u)

      this
    }

    /**
     * Update the state of an agent in this mixture.
     *
     * @param agent the agent to modify.
     * @param state the new state of `agent`.
     * @return this mixture with the state of `agent` changed to
     *         `state`.
     */
    def updateAgentState(agent: Agent, state: AgentState): Mixture = {
      checkpointAgent(agent)
      agent.state = state
      mark(agent)
      this
    }

    /**
     * Update the state of a site of some agent in this mixture.
     *
     * @param agent the agent containing the site to modify.
     * @param siteIdx the index of the site to modify.
     * @param state the new state of the site to modify.
     * @return this mixture with the state of the site at `siteIdx`
     *         changed to `state`.
     */
    def updateSiteState(
      agent: Agent, siteIdx: SiteIndex, state: SiteState): Mixture = {
      checkpointAgent(agent)
      agent.sites(siteIdx).state = state
      mark(agent)
      this
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
    def ++=(that: Mixture): Mixture = {
      var a: Agent = that._head
      var last: Agent = null
      while (a != null) {
        a.mixture = this
        unmark(a)
        mark(a)
        last = a
        a = a.next
      }
      if (last != null) {
        if (this._head != null) this._head.prev = last
        last.next = this._head
        this._head = that._head
      }
      _length += that._length
      this
    }

    /**
     * Generates and appends `x - 1` copies of this mixture to this
     * mixture.
     */
    def *=(x: Int): Mixture =
      if (x <= 0) throw new IllegalArgumentException(
        "attempt to create a negative number of copies of a mixture")
      else {
        val m = Mixture()
        for (i <- 1 until x) {
          m ++= this.copy
        }
        this ++= m
      }

    /**
     * Generates `x` concatenated copies of this mixture and returns
     * the result.
     */
    def *(x: Int): Mixture =
      if (x == 0) Mixture()
      else if (x < 0) throw new IllegalArgumentException(
        "attempt to create a negative number of copies of a mixture")
      else {
        val m = this.copy
        m *= x
      }


    // -- Core Seq[Agent] API --
    @inline def apply(idx: Int): Agent = {
      if (idx >= _length) throw new IndexOutOfBoundsException
      (iterator drop idx).next
    }
    @inline def iterator: Iterator[Agent] = new MixtureIterator
    @inline override def length: Int = _length


    // -- Extra Seq[Agent] API --
    @inline override def foreach[U](f: Agent => U): Unit = {
      var u = _head
      while (u != null) {
        f(u)
        u = u.next
      }
    }

    override def toString = iterator.mkString("", ",", "")
  }

  object Mixture {

    /** A class representing potential links between [[Mixture.Site]]s. */
    // FIXME: This can be simplified by representing stubs as loops (self-links).
    //
    // RHZ: Do we need to simplify it that much? Representing stubs as self-links
    // is conceptually wrong and misleading for the guy reading the code
    //
    // stucki: Yes, that is true, and that is why I did not implement
    // it so far.  Using self-links I think we could avoid some
    // overhead (essentially unnecessary object creation and pattern
    // matches) but I'm not sure how substantial the gain in
    // performance would be and in Scala using "Stub" is certainly
    // more clear -- so let's keep it as it is.  In C/C++ this is a
    // bit different, because there are no native "variant types" and
    // so using self-links would be more efficient and might even lead
    // to less complex code.
    sealed abstract class Link {
      // FIXME!
      override def toString = this match {
        case Stub => ""
        case Linked(a, s, l) =>
          "!" + a.state + "." + a.sites(s).state + "." + l
      }
    }

    /** An object representing stubs, i.e. unconnected [[Mixture.Site]]s. */
    case object Stub extends Link

    /**
     * A class representing actual links links between [[Mixture.Site]]s.
     *
     * Instances of this class also hold the [[LinkState]] in the
     * direction from the source site (i.e. the site that stores the
     * instance) to the target site (i.e. the site that is pointed to
     * by the instance).
     *
     * A few words about why target sites are stored in a "relative"
     * fashion, i.e. as (agent, site index) pairs rather than just a
     * reference to the target site.  There are two main reasons for
     * this:
     *
     *  1) Links between sites in [[Mixture]]s are predominantly
     *     created by applying an action (see [[Rules#Action]]) to a
     *     mixture.  An action is a function that takes a mixture and
     *     a sequence of agents as its input and updates the mixture.
     *     Since the agents that are to be updated are parameters of
     *     the action, the concrete sites to be modified are not know
     *     when the action is created, instead the action refers to
     *     the sites just by their index w.r.t. the corresponding
     *     (formal) agent parameter.  To simplify the operations that
     *     actions perform on agents and sites it makes sense to store
     *     links in an analogous way.
     *
     *  2) Copying mixtures is slightly cumbersome because all the
     *     inter-agent and agent-mixture pointers have to be updated
     *     in the copy of the mixture so as to refer to the copies of
     *     the original agents rather than the originals themselves.
     *     Storing links in a "relative" fashion removes some of this
     *     complexity, since the position of sites w.r.t. their agents
     *     do not change.
     *
     * @param agent the target agent of this link.
     * @param site the index of the target site in the target agent of
     *        this link.
     * @param state the state of the link from source to target.
     */
    final case class Linked(agent: Agent, site: SiteIndex, state: LinkState)
        extends Link

    /**
     * A class representing sites of [[Mixture.Agent]]s.
     *
     * FIXME: `state` and `link` should probably be read-only outside
     * of [[Mixture]].
     *
     * @param state the state of this site
     * @param link whether and how this site is linked to another site.
     */
    final case class Site(var state: SiteState, var link: Link = Stub) {
      override def toString = state.toString + link
    }

    /**
     * A class representing agents in [[Mixture]]s.
     *
     * This class has two roles: it acts as a container for agent
     * information (agent state, interfaces, lift sets) and, at the
     * same time, as a cell in a doubly linked list that forms the
     * mixture.  The reason for this double-role is that in this way
     * an [[Agent]]s can be pointed to directly (i.e. without an
     * extra layer of indirection, e.g. in
     * [[PartialEmbeddings#PartialEmbedding]s) and removed from the
     * mixture efficiently (i.e. without the need of a back-pointer
     * from an agent to its list cell).
     *
     * TODO: Should we make this an inner class of [[Mixture]]?
     *
     * @param state the state of this agent.
     * @param sites the interface of this agent.
     * @param liftSet lift set for component embeddings.
     * @param mixture a reference to the [[Mixture]] this agent belongs to.
     * @param next a reference to the next agent in the [[Mixture]]
     *        this agent belongs to.
     * @param prev a reference to the previous agent in the [[Mixture]]
     *        this agent belongs to.
     */
    final case class Agent protected[Mixture] (
      var state: AgentState,
      val sites: Array[Site],
      val liftSet: mutable.HashSet[(Pattern.Agent, ComponentEmbedding)] =
        new mutable.HashSet())
        extends Seq[Site] {

      protected[Mixture] var mixture: Mixture = null
      protected[Mixture] var next: Agent = null
      protected[Mixture] var prev: Agent = null

      /** A reference to a copy of this agent (used by [[Mixture]]`.copy`). */
      protected[Mixture] var copy: Agent = null

      /**
       * Marker flag for agents to be considered checked (used to
       * track clashes in actions, updates made by actions, etc.).
       */
      protected[Mixture] var _marked: Boolean = false

      /**
       * Make a checkpoint copy of a given agent.
       *
       * @return a copy of this agent (sharing states and links).
       */
      protected[Mixture] def checkpointCopy: Agent = {
        // First allocate an "empty" agent `v` tracking `this`.
        val vSites = new Array[Site](this.sites.size)
        val v = new Agent(this.state, vSites)
        v.mixture = this.mixture
        v.prev = this.prev
        v.next = this.next
        v.copy = this

        // Now setup the interfaces of `v`
        for (i <- this.sites.indices) {
          val s = this.sites(i)
          v.sites(i) = Site(s.state, s.link)
        }
        v
      }

      /**
       * Marker flag for agents to be considered checked (used to
       * track clashes in actions, updates made by actions, etc.).
       */
      @inline def marked: Boolean = _marked

      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @param site the index of the site whose neighbor we try to find.
       * @return `Some(a, s)`, where `a` is the neighboring agent of
       *         this site and `s` is the index of the neighboring site
       *         in `a`, or `None` if this site is not connected.
       */
      // Wow! BE!! =)
      @inline def neighbour(site: SiteIndex): Option[(Agent, SiteIndex)] =
        sites(site).link match {
          case Linked(a, s, _) => Some((a, s))
          case _ => None
        }

      /**
       * Register a component embedding in the lift set of this agent.
       *
       * @param u the pre-image of this agent in the component embedding.
       * @param ce the component embedding to add.
       */
      def addLift(u: Pattern.Agent, ce: ComponentEmbedding) {
        liftSet += ((u, ce))
      }

      /**
       * Remove a component embedding from the lift set of this agent.
       *
       * @param u the pre-image of this agent in the component embedding.
       * @param ce the component embedding to remove.
       */
      def removeLift(u: Pattern.Agent, ce: ComponentEmbedding) {
        liftSet -= ((u, ce))
      }

      /**
       * Check all component embeddings in the lift set for
       * consistency and remove those that are no longer valid.
       */
      def pruneLifts {
        val invalidLifts = liftSet filter { p => !(p._1 matches this) }
        for ((u, ce) <- invalidLifts) {
          ce.component.removeEmbedding(ce)
        }
      }

      // -- Core Seq[Site] API --
      @inline def apply(idx: Int): Site = sites(idx)
      @inline def iterator: Iterator[Site] = sites.iterator
      @inline def length: Int = sites.length

      // -- Extra Seq[Site] API --
      @inline override def foreach[U](f: Site => U): Unit =
        sites.foreach(f)

      // -- Equals API --
      /** TODO: Is this method redundant? */
      override def canEqual(that: Any) = that.isInstanceOf[Agent]

      // -- Any API --
      /**
       * Tests whether the argument (`that`) is a reference to the
       * receiver object (`this`).
       *
       * NOTE: There are multiple reasons fro overriding the `equals`
       * method in this class:
       *
       *  1) Since this class is also a sequence of [[Site]]s (it
       *     extends `Seq[Site]`), it inherits its `equals`
       *     implementation from [[scala.collection.GenSeqLike]].
       *     This method eventually ends up calling [[Site.equals]]
       *     (via [[scala.collection.IterableLike.sameElements]]).
       *     But because sites contain links, which in turn may
       *     contain references to [[Agent]]s (i.e. if they are
       *     instances of [[Linked]]), a call to the default `equals`
       *     method may end up in a recursive loop, eventually causing
       *     a stack overflow.
       *
       *  2) For efficiency.  We really consider different instances
       *     of this class as different agents.  If you want to test
       *     for structural equality, use [[Agent.isEquivTo]] instead.
       *
       * @return `true` if the argument is a reference to the receiver
       *         agent; `false` otherwise.
       */
      override def equals(that: Any): Boolean =
        that.isInstanceOf[Agent] && (this eq that.asInstanceOf[Agent])

      /**
       * Calculate a hash code value for the agent.
       *
       * NOTE: The reasons for overriding the `hashCode` method in
       * this class are the same as those mentioned in
       * [[Agent.equals]].
       *
       * TODO: Is it OK to rely on
       * [[java.lang.System.identityHashCode]]?  A possible
       * alternative would be, to use a counter in the companion
       * object to provide unique (up to counter wrap-around) hash
       * codes when creating an instance of this class.
       *
       * @return the hash code value for this agent.
       */
      override def hashCode(): Int =
        java.lang.System.identityHashCode(this)

      override def toString() = state + sites.mkString("(", ",", ")")
    }


    /** Creates an empty mixture. */
    @inline def apply(): Mixture = new Mixture

    // RHZ: I don't like very much the idea of creating mixtures from patterns
    // since Scala can't apply 2 implicit conversions in a row (modulo some
    // weird trickery). I'd prefer to create mixtures from strings, as we do
    // with patterns

    /** Converts a pattern into a mixture. */
    def apply(pattern: Pattern): Mixture = {
      val m = new Mixture
      for (c <- pattern.components) {

        if (!c.isComplete) throw new IllegalArgumentException(
          "attempt to create mixture from incomplete pattern: " + pattern)

        // Allocate "empty" copies of agents in this component
        val as = new Array[Agent](c.agents.size)
        for (u <- c.agents) {
          val v = Agent(u.state, new Array[Site](u.sites.size))
          m += v
          as(u.index) = v
        }

        // Setup the interfaces of the agents in the component
        for (u <- c.agents) {
          var i = 0
          for (s <- u.sites) {
            val l = s.link match {
              case Pattern.Linked(u, j, l) => Linked(as(u.index), j, l)
              case Pattern.Stub => Stub
              case _ => throw new IllegalArgumentException(
                "attempt to create mixture with an undefined or wildcard link")
            }
            as(u.index).sites(i) = Site(s.state, l)
            i += 1
          }
        }
      }
      m
    }

    /** Converts a pattern into a mixture. */
    implicit def patternToMixture(pattern: Pattern) = apply(pattern)
  }

  /** Default Mixture of the enclosing model. */
  val mix = Mixture()
}
