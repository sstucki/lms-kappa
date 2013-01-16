package kappa

import scala.collection.mutable

import scala.language.implicitConversions

trait Mixtures {
  mixtures: LanguageContext with Patterns with Embeddings =>

  /**
   * A class representing mixtures in [[kappa.Model]]s (i.e. site
   * graphs).
   *
   * Mixtures are mutable and organized as doubly linked lists of
   * [[Mixture.Agent]]s.  Operations on mixtures are destructive in
   * that the mixture is modified in place and if it appears as a
   * parameter (e.g. as an argument to the `++=` method) it might be
   * destroyed in the process.  No attempt is made to guarantee the
   * consistency of such zombie mixtures, and indeed not much effort
   * is made to guarantee the consistency of mixtures overall.  In
   * general, mixtures should be handled with care and, if possible,
   * only through actions + embeddings.
   * 
   * @constructor create an empty Mixture.
   */
  class Mixture {

    import Mixture._

    /** First agent in the doubly-linked list representing the mixture. */
    private var head: Agent = null

    /** The number of elements in this mixture. */
    var size: Int = 0

    class MixtureIterator extends Iterator[Agent] {
      private var nextAgent: Agent = head
      def next = {
        val n = nextAgent
        if (n != null) nextAgent = n.next
        n
      }
      def hasNext = nextAgent != null
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
      var u: Agent = head
      val that = new Mixture
      that.size = this.size

      // First allocate "empty" agents for `that`
      var p: Agent = null
      while(u != null) {
        val sites = new Array[Site](u.sites.size)
        val v = Agent(u.state, sites, mixture = that, prev = p)
        if (p == null) { that.head = v } else { p.next = v }
        u.copy = v
        p = v
        u = u.next
      }

      // Now setup the interfaces of the agents in `that`
      u = head
      while(u != null) {
        val v = u.copy
        for (i <- 0 until u.sites.size) {
          val s = u.sites(i)
          val l = s.link match {
            case Linked(a, s, l) => Linked(a.copy, s, l)
            case Stub => Stub
          }
          v.sites(i) = Site(s.state, l)
        }
        u = u.next
      }

      // Reset `copy` references in the agents of `this` to avoid
      // memory leaks.
      u = head
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
      agent.next = head
      if (head != null) head.prev = agent
      head = agent
      size += 1
      this
    }

    /** Create a [[Mixture.Agent]] and add it to this mixture. */
    def +=(state: AgentState, siteStates: Seq[SiteState]): Mixture = {
      val n = siteStates.size
      var i: Int = 0
      val sites = new Array[Site](n)
      val it = siteStates.iterator
      while (it.hasNext) {
        sites(i) = new Site(it.next)
      }
      this += (new Agent(state, sites))
    }

    /** Remove a single agent to this mixture. */
    def -=(agent: Agent): Mixture = {
      val ap = agent.prev
      val an = agent.next
      if (ap != null) ap.next = an
      if (an != null) an.prev = ap
      if (head == agent) head = an
      size -= 1
      this
    }

    /** Connect two sites in this mixture. */
    def connect(a1: Agent, s1: SiteIndex, l1: LinkState,
                a2: Agent, s2: SiteIndex, l2: LinkState): Mixture = {
      a1.sites(s1).link = Linked(a2, s2, l1)
      a2.sites(s2).link = Linked(a1, s1, l2)
      this
    }

    /** Disconnect two sites in this mixture. */
    def disconnect(a1: Agent, s1: SiteIndex,
                   a2: Agent, s2: SiteIndex): Mixture = {
      a1.sites(s1).link = Stub
      a2.sites(s2).link = Stub
      this
    }

    /**
     * Concatenate this mixture and another one.
     *
     * The doubly-linked list representing the mixture `that` will be
     * appended to this mixture.  After this concatenation `that`
     * becomes invalid and should not be operated on any longer. 
     */
    def ++=(that: Mixture): Mixture = {
      var a: Agent = that.head
      var last: Agent = null
      while (a != null) {
        a.mixture = this
        last = a
        a = a.next
      }
      if (last != null) {
        if (this.head != null) this.head.prev = last
        last.next = this.head
        this.head = that.head
      }
      size += that.size
      this
    }

    /** Generates and returns `x` concatenated copies of this mixture. */
    def *(x: Int): Mixture =
      if (x == 0) Mixture()
      else if (x < 0) throw new IllegalArgumentException(
        "attempt to create a negative number of copies of a mixture")
      else {
        val m = Mixture()
        for (i <- 1 until x) {
          m ++= this.copy
        }
        this ++= m
      }

    // Seq methods
    @inline def apply(idx: Int): Agent = {
      if (idx >= size) throw new IndexOutOfBoundsException
      (iterator drop idx).next
    }
    @inline def iterator: Iterator[Agent] = new MixtureIterator
    @inline def length: Int = size

    override def toString = iterator.mkString("", ",", "")
  }

  object Mixture {

    /** A class representing links between [[Mixture.Site]]s. */
    // FIXME: This can be simplified by representing stubs as loops (self-links).
    sealed abstract class Link {
      // FIXME!
      override def toString = this match {
        case Stub => ""
        case Linked(a, s, l) =>
          "!" + a.state + "." + a.sites(s).state + l
      }
    }
    case object Stub extends Link
    case class Linked(agent: Agent, site: SiteIndex,
                      state: LinkState) extends Link

    /**
     * A class representing sites of [[Mixture.Agents]]s.
     *
     * @param state the [[SiteState]] of this site
     * @param link whether this site is linked to another site.
     * @param stateLiftSet lift set for site state modifications
     * @param linkLiftSet lift set for link modifications
     */
    case class Site(
      var state: SiteState,
      var link: Link = Stub,
      val stateLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet(),
      val linkLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet()) {

      override def toString = state.toString + link
    }

    /**
     * A class representing agents in [[Mixture]]s.
     *
     * @param state the [[AgentState]] of this agent.
     * @param sites the interface of this agent.
     * @param stateLiftSet lift set for agent state modifications
     * @param mixture a reference to the [[Mixture]] this agent belongs to.
     * @param next a reference to the next agent in the [[Mixture]]
     *        this agent belongs to.
     * @param next a reference to the previous agent in the [[Mixture]]
     *        this agent belongs to.
     */
    case class Agent(
      var state: AgentState,
      val sites: Array[Site],
      val stateLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet(),
      var mixture: Mixture = null,
      var next: Agent = null,
      var prev: Agent = null) {

      /** A reference to a copy of this agent (used by [[Mixture]]`.copy`). */
      protected[Mixture] var copy: Agent = null

      /**
       * Returns the neighbor of this site if it is connected.
       *
       * @return `Some(x)`, where `x` is the neighbor of this site, if
       *         this site is not connected, and `None` otherwise.
       */
      @inline def neighbor(site: SiteIndex) : Option[(Agent, SiteIndex)] =
        sites(site).link match {
          case Linked(a, s, _) => Some((a, s))
          case _ => None
        }

      override def toString() = state + sites.mkString("(", ",", ")")

      // Seq methods
      @inline def apply(idx: Int): Site = sites(idx)
      @inline def iterator: Iterator[Site] = sites.iterator
      @inline def length: Int = sites.length
    }

    /** Creates an empty mixture. */
    @inline
    def apply(): Mixture = new Mixture

    /** Converts a pattern into a mixture. */
    def apply(pattern: Pattern): Mixture = {
      val m = new Mixture
      for (c <- pattern.components) {

        // Allocate "empty" copies of agents in this component
        val as = new Array[Agent](c.agents.size)
        for (u <- c.agents) {
          val v = new Agent(u.state, new Array[Site](u.sites.size))
          m += v
          as(u.index) = v
        }

        // Setup the interfaces of the agents in the component
        for (u <- c.agents) {
          var j = 0
          for (s <- u.sites) {
            val l = s.link match {
              case Pattern.Linked(a, s, l) => Linked(as(a.index), s, l)
              case Pattern.Stub => Stub
              case _ => throw new IllegalArgumentException(
                "attempt to convert pattern with undefined or wildcard " +
                "link to mixture")
            }
            as(u.index).sites(j) = new Site(s.state, l)
          }
        }
      }
      m
    }

    /** Converts a pattern into a mixture. */
    implicit def patternToMixture(pattern: Pattern) = apply(pattern)
  }
}
