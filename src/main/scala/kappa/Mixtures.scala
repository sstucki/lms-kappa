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
     * Make a copy (or rather a clone) of this mixture.
     *
     * The resulting mixture is completely independent of the original
     * and can hence be operated on without the fear of destroying the
     * original.
     *
     * @return a clone of this mixture.
     */
    // FIXME: Copying/cloning a Mixture is more complex than it might
    // appear at first.  The various pointers/references in the target
    // Mixture must be set such that they point to the appropriate
    // agents/sites in the target Mixtures, rather than the
    // agents/sites in the source mixture.  Hence, we need to traverse
    // the entire graph, following both the `prev` and `next` pointers
    // as well as all the `agent` pointers in the links of the agents
    // sites, and copy the nodes as we encounter them.  To keep track
    // of which nodes have already been copied, and where to, we need
    // a to keep a special pointer in the agents of the source
    // Mixture.  Once we're done, these need to be reset to `null` in
    // order to enable future copy operations.
    def copy = this

    /** Add a single agent to this mixture. */
    def +=(agent: Agent) = {
      agent.mixture = this
      agent.prev = null
      agent.next = head
      head.prev = agent
      head = agent
      this
    }

    /** Create a [[Mixture.Agent]] and add it to this mixture. */
    def +=(state: AgentState, siteStates: Seq[SiteState]) {
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
    def -=(agent: Agent) = {
      val ap = agent.prev
      val an = agent.next
      if (ap != null) ap.next = an
      if (an != null) an.prev = ap
      if (head == agent) head = an
      this
    }

    /** Connect two sites in this mixture. */
    def connect(a1: Agent, s1: SiteIndex, l1: LinkState,
                a2: Agent, s2: SiteIndex, l2: LinkState) = {
      a1.sites(s1).link = Linked(a2, s2, l1)
      a1.sites(s1).link = Linked(a2, s2, l1)
      this
    }

    /** Disconnect two sites in this mixture. */
    def disconnect(a1: Agent, s1: SiteIndex, a2: Agent, s2: SiteIndex) = {
      a1.sites(s1).link = Stub
      a1.sites(s1).link = Stub
      this
    }

    /**
     * Concatenate this mixture and another one.
     *
     * The doubly-linked list representing the mixture `that` will be
     * appended to this mixture.  After this concatenation `that`
     * becomes invalid and should not be operated on any longer. 
     */
    def ++=(that: Mixture) = {
      var a: Agent = that.head
      var last: Agent = null
      while (a != null) {
        a.mixture = this
        last = a
        a = a.next
      }
      if (last != null) {
        this.head.prev = last
        this.head = that.head
      }
      this
    }

    /** Generates and returns `x` copies of this mixture. */
    // FIXME: Dummy, implement! Or should this be implemented in
    // pattern instead?
    def *(x: Int) = this

    def iterator = new MixtureIterator

  }

  object Mixture {

    /** A class representing links between [[Patterns.Site]]s. */
    // FIXME: This can be simplified by representing stubs as loops (self-links).
    sealed abstract class Link
    case object Stub extends Link
    case class Linked(agent: Agent, site: SiteIndex,
                      state: LinkState) extends Link

    // Sites
    case class Site(
      //val index: SiteIndex,
      var state: SiteState,
      var link: Link = Stub,
      val stateLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet(),
      val linkLiftSet: mutable.HashSet[Embedding] = new mutable.HashSet()) {

      }

    /**
     * A class representing agents in [[Mixture]]s.
     */
    case class Agent(var state: AgentState, val sites: Array[Site],
                     var mixture: Mixture = null, var next: Agent = null,
                     var prev: Agent = null) {

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
    }

    /** Creates an empty mixture. */
    def apply(): Mixture = new Mixture

    /** Converts a pattern into a mixture. */
    // FIXME: Implement!
    def apply(pattern: Pattern): Mixture = Mixture()

    /** Converts a pattern into a mixture. */
    implicit def patternToMixture(pattern: Pattern) = apply(pattern)
  }
}
