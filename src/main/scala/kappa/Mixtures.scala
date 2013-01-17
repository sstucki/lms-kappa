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
   * parameter (e.g. as an argument to the [[++=]] method) it might be
   * destroyed in the process.  No attempt is made to guarantee the
   * consistency of such zombie mixtures, and indeed not much effort
   * is made to guarantee the consistency of mixtures overall.  In
   * general, mixtures should be handled with care and, if possible,
   * only through actions + embeddings.
   * 
   * @constructor create an empty Mixture.
   */
  class Mixture {
    // RHZ any particular reason to make this like a cons cell instead of just
    // wrapping around an Array of Agents? I find it quite cumbersome this way

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
          var s = u.sites(i)
          val l = s.link match {
            // FIXME this is wrong but it was wrong before as well!
            // in the sense that the agent the link referred to was not the one
            // somewhere in a cons cell linked by that, but an agent floating in
            // the middle of nowhere
            case Linked(s, l) => Linked(s, l)
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

    // RHZ These two methods should belong to Site
    // NB connect should return the link and the link should have a method
    // withState for cooler syntax

    /** Connect two sites in this mixture. */
    def connect(s1: Site, l1: LinkState,
                s2: Site, l2: LinkState): Mixture = {
      s1.link = Linked(s2, l1)
      s2.link = Linked(s1, l2)
      this
    }

    // This should receive only one site as input, as in KaSim, to easily handle
    // wildcard destruction
    /** Disconnect two sites in this mixture. */
    def disconnect(s1: Site,
                   s2: Site): Mixture = {
      s1.link = Stub
      s2.link = Stub
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
    //
    // RHZ: Do we need to simplify it that much? Representing stubs as self-links
    // is conceptually wrong and misleading for the guy reading the code
    sealed abstract class Link {
      // FIXME!
      override def toString = this match {
        case Stub => ""
        case Linked(s, l) =>
          "!" + s.agent.state + "." + s.state + "." + l
      }
    }
    case object Stub extends Link
    case class Linked(site: Site,
                      state: LinkState) extends Link

    /**
     * A class representing sites of [[Mixture.Agent]]s.
     *
     * @param state the state of this site
     * @param link whether this site is linked to another site.
     * @param stateLiftSet lift set for site state modifications
     * @param linkLiftSet lift set for link modifications
     */
    case class Site(
      var state: SiteState,
      var link: Link = Stub,
      val stateLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet(),
      val linkLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet()) {

      // RHZ: should we reference the parent Agent in Mixtures as well?
      var agent: Agent = null

      override def toString = state.toString + link

      @inline def neighbor : Option[Site] =
        link match {
          case Linked(s, _) => Some(s)
          case _ => None
        }
    }

    /**
     * A class representing agents in [[Mixture]]s.
     *
     * @param state the state of this agent.
     * @param sites the interface of this agent.
     * @param stateLiftSet lift set for agent state modifications
     * @param mixture a reference to the [[Mixture]] this agent belongs to.
     * @param next a reference to the next agent in the [[Mixture]]
     *        this agent belongs to.
     * @param prev a reference to the previous agent in the [[Mixture]]
     *        this agent belongs to.
     */
    case class Agent(
      var state: AgentState,
      val sites: Array[Site],
      val stateLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet(),
      // RHZ: Maybe we should make this type-safe as well
      // Since it's mutable I'll just turn a blind eye to it for now
      var mixture: Mixture = null,
      var next: Agent = null,
      var prev: Agent = null) {

      /** A reference to a copy of this agent (used by [[Mixture]]`.copy`). */
      protected[Mixture] var copy: Agent = null

      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @param site the index of the site whose neighbor we try to find.
       * @return `Some(x)`, where `x` is the neighbor of this site, if
       *         this site is not connected, and `None` otherwise.
       */
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
          for (si <- 0 until u.sites.size) {
            val s = u.sites(si)
            val l = s.link match {
              case Pattern.Linked(s, l) => Linked(as(s.agent.index).sites(si), l)
              case Pattern.Stub => Stub
              case _ => throw new IllegalArgumentException(
                "attempt to create mixture with an undefined or wildcard link")
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
