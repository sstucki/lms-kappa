package kappa

import language.implicitConversions
import scala.language.postfixOps

trait Patterns {
  this: LanguageContext with Parser with Rules with Symbols with Mixtures
                        with PartialEmbeddings =>

  /**
   * A class representing patterns in [[Model]]s (i.e. site graphs).
   *
   * ''WARNING'': For convenience, this class provides the interface
   * of a `Seq[Pattern.Agent]`.  However, using some methods from the
   * `Seq` API might not result in the expected behavior.  E.g. `++`
   * will return a `Seq[Pattern.Agent]` rather than the expected
   * `Pattern`.
   *
   * @constructor create a new Pattern.
   * @param components the connected components of this pattern.
   * @param agents a vector of [[Pattern.Agent]]s that can be used
   *        to query the individual agents in this pattern given their
   *        position within the pattern (i.e. in the order in which
   *        they were added to the pattern).
   */
  final class Pattern private (
    val components: Vector[Pattern.Component],
    val agents: Vector[Pattern.Agent],
    val siteGraphString: String)
        extends Seq[Pattern.Agent]
  {
    import Pattern._

    /**
     * Return the (overestimated) number of matchings of this pattern
     * in the target mixture
     */
    def inMix: Double = (components map (_.count)).product


    // RHZ: @Sandro Beware that connect has side-effects and therefore
    // siteGraphString won't return the correct string after any of these
    // modifications. An easy fix is to create new Patterns giving
    // an empty string as siteGraphString when connecting or merging.
    // That doesn't solve the problem that the original sites will have
    // their links changed when connecting though.
    override def toString =
      if (!siteGraphString.isEmpty) siteGraphString
      else iterator.mkString("", ",", "")

    // Action constructor
    def -> (rhs: Pattern) = new Action(this, rhs)


    // -- Core Seq[Agent] API --
    @inline def apply(idx: Int): Agent = agents(idx)
    @inline def iterator: Iterator[Agent] = agents.iterator
    @inline def length: Int = agents.length

    @inline def apply(ci: ComponentIndex, ai: AgentIndex): Agent =
      components(ci).agents(ai)


    /* Update the pattern pointers in the components. */
    for ((c, i) <- components.zipWithIndex) {
      c._pattern = this
      c._index = i
    }
  }

  /** Companion object of the [[Patterns.Pattern]] class. */
  // RHZ: Why not work with path-dependent types? There are things that can only
  // be done on agents in the same pattern (like connecting) and path-dependent
  // types let us enforce that with the type system... And for the things that
  // can be done on any pair of components or agents, we use the supertype
  // Pattern#Component or Pattern#Agent
  //
  // sstucki: Yes, but things are a bit more subtle than that.  The
  // way Patterns are built right now is that we first build a
  // collection of Agents and then pass them to the Pattern
  // constructor.  That is not possible when Agent is an inner class
  // of Pattern.  You can simply not create an inner Agent without
  // first creating the Outer pattern.  Now if we were to make Pattern
  // more mutable, this would work (just create a Pattern and have
  // e.g. a ":+" that takes an agent state and creates an agent).  We
  // would have to build sites similarly (if we want them to be inner
  // classes of Patterns or even Agents).  Maybe that is the way to
  // go, but as things look right now it's not possible.  Btw. it is
  // possible to make Agents inner classes in Mixtures as these are
  // already built in this way.  I added a comment about this in the
  // Mixture class.
  object Pattern {

    /** A class representing links between [[Pattern.Site]]s. */
    sealed abstract class Link {

      /**
       * Compare this link against a [[Mixtures#Mixture.Link]].
       *
       * @return `true` if this site matches `that`.
       */
      def matches(that: Mixture.Link): Boolean = (this, that) match {
        case (Undefined, _) => true
        case (Stub, Mixture.Stub) => true
        case (Wildcard(a1, s1, l1), Mixture.Linked(a2, s2, l2)) =>
          (a1 map (_ matches a2.state) getOrElse true) &&
          (s1 map (_ matches a2.sites(s2).state) getOrElse true) &&
          (l1 map (_ matches l2) getOrElse true)
        case (Linked(_, l1), Mixture.Linked(_, _, l2)) =>
          l1 matches l2
        case _ => false
      }

      override def toString = this match {
        case Undefined => "?"
        case Wildcard(a, s, l) =>
          "!" + (a getOrElse "_") + "." + (s getOrElse "_") + "." + (l getOrElse "_")
        case Stub => ""
        case Linked(s, l) =>
          "!" + s.agent.state + "." + s.state + "." + l
      }
    }

    /** An object representing an undefined link at a [[Pattern.Site]]. */
    case object Undefined extends Link

    /** An object representing stubs, i.e. unconnected [[Pattern.Site]]s. */
    case object Stub extends Link

    /**
     * A class representing actual links links between [[Pattern.Site]]s.
     *
     * Instances of this class also hold the
     * [[LanguageContext#LinkState]] in the direction from the source
     * site (i.e. the site that stores the instance) to the target
     * site (i.e. the site that is pointed to by the instance).
     *
     * As opposed to [[Mixtures#Mixture.Link]], the target sites are stored
     * in an "absolute" fashion, i.e. as just a reference to the target site.
     *
     * @param agent the target agent of this link.
     * @param site the index of the target site in the target agent of
     *        this link.
     * @param state the state of the link from source to target.
     */
    final case class Linked(site: Site, state: LinkState)
        extends Link

    /**
     * A class representing a wildcard link at a [[Pattern.Site]].
     *
     * The class allows to specify constraints on the type and state
     * of the link target, i.e. the states of the link, the target
     * site and the target agent that the wildcard matches.
     *
     * @param agentState the state a matching target agent should have.
     * @param siteState the state a matching target site should have.
     * @param linkState the state a matching link should have.
     */
    final case class Wildcard(
      agentState: Option[AgentState],
      siteState: Option[SiteState],
      linkState: Option[LinkState]) extends Link

    /**
     * A class representing sites in [[Pattern.Agent]]s.
     */
    final class Site private (
      val state: SiteState,
      var link: Link = Undefined)
    {
      // RHZ: There's redundant information in sites since the state
      // contains the site name sym which is the same as index

      /** Internal reference to the agent this site belongs to. */
      protected[Pattern] var _agent: Agent = null

      /** The agent this site belongs to. */
      @inline def agent =
        if (_agent == null) throw new NullPointerException(
          "attempt to access parent agent of orphan site")
        else _agent

      /** The index of the agent within the component. */
      protected[Pattern] var _index: SiteIndex = -1
      @inline def index =
        if (_index < 0) throw new NullPointerException(
          "I have no index =(")
        else _index

      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @return `Some(x)`, where `x` is the neighboring site of this
       *         site, if this site is connected, and `None` otherwise.
       */
      @inline def neighbor: Option[Site] =
        link match {
          case Linked(s, _) => Some(s)
          case _ => None
        }

      /**
       * Compare this site against a [[Mixtures#Mixture.Site]].
       *
       * @return `true` if this site matches `that`.
       */
      def matches(that: Mixture.Site): Boolean =
        (this.state matches that.state) && (this.link matches that.link)

      override def toString = state.toString + link
    }

    // RHZ: Why not as part of the class?
    //
    // sstucki: because factory methods and custom extractors are
    // defined in companion objects in Scala.  See
    // http://daily-scala.blogspot.ch/2009/09/factory-methods-and-companion-objects.html
    // and http://www.scala-lang.org/node/112
    //
    // RHZ: Thanks for the references

    /** Companion object of [[Site]] containing factory methods */
    object Site {
      def apply(state: SiteState) = new Site(state)
      def apply(state: SiteState, link: Link) = new Site(state, link)
      def unapply(site: Site) = (site.state, site.link)
    }

    /**
     * A class representing agents in [[Pattern.Component]]s.
     *
     * @param state the state of the agent.
     * @param sites the sites of the agent.
     */
    case class Agent(val state: AgentState, val sites: Vector[Site])
    {
      /** The component this agent belongs to. */
      // RHZ: made this type-safe
      //
      // sstucki: No, you made it into an Option, that is no more (or
      // less) type-safe than a (null) pointer, it's just more
      // expressive. ;-) But since this pointer is not (supposed to
      // be) used in any interface, this expressiveness isn't really
      // useful.  I reverted it and made the pointer protected.  Now
      // expressive error messages on the other hand are very useful,
      // so I kept that.
      //
      // RHZ: I disagree! Option is type-safe compared to a null pointer
      // since the compiler forces you to do the "if (ptr == null) ..."
      // That's what I mean by type-safe
      // Now I agree that a getter is a good type-safe solution too
      // since it guarantees that the returned pointer can't be null
      protected[Pattern] var _component: Component = null
      @inline def component =
        if (_component == null) throw new NullPointerException(
          "attempt to access parent component of orphan agent")
        else _component

      /** The index of the agent within the component. */
      protected[Pattern] var _index: AgentIndex = -1
      @inline def index =
        if (_index < 0) throw new NullPointerException(
          "I have no index =(")
        else _index

      /**
       * Compare this agent against a [[Mixtures#Mixture.Agent]].
       *
       * @return `true` if this agent matches `that`.
       */
      def matches(that: Mixture.Agent): Boolean =
        that.sites.size == this.sites.size &&
        (this.state matches that.state) &&
        (this.sites zip that.sites forall { case (s1, s2) => s1 matches s2 })

      override def toString() = state + sites.mkString("(", ",", ")")

      // Register sites.
      for ((s, i) <- sites.zipWithIndex) {
        s._agent = this
        s._index = i
      }
    }

    /**
     * A class representing connected components of [[Patterns.Pattern]]s
     * (i.e. connected components of site graphs).
     *
     * ''WARNING'': For convenience, this class provides the
     * interface of a `Seq[Pattern.Agent]`.  However, using some
     * methods from the `Seq` API might not result in the expected
     * behavior.  E.g. `++` will return a `Seq[Pattern.Agent]` rather
     * than the expected `Pattern.Component`.
     *
     * @param agents the agents in this component
     */
    case class Component(val agents: Vector[Agent]) extends Seq[Agent]
    {
      /** The pattern this component belongs to. */
      protected[Pattern] var _pattern: Pattern = null
      def pattern =
        if (_pattern == null) throw new NullPointerException(
          "attempt to access parent pattern of orphan component")
        else _pattern

      /** The index of the component within the pattern. */
      protected[Pattern] var _index: ComponentIndex = -1
      def index =
        if (_index < 0) throw new NullPointerException(
          "attempt to retrieve in-pattern index of orphan component")
        else _index

      /**
       * Return the number of matchings of this pattern component in
       * the target mixture.
       */
      def count: Int = 1

      /**
       * Return all the (partial) embeddings from this pattern
       * component in a given mixture.
       *
       * RHZ: The idea is to use this method to find all the initial
       * embeddings?
       *
       * @return all the partial embeddings from `this` in `that`.
       */
      def partialEmbeddingsIn(that: Mixture): Seq[PartialEmbedding] =
        (for (u <- this; v <- that) yield PartialEmbedding(u, v)).flatten


      // -- Core Seq[Agent] API --
      @inline def apply(idx: Int): Agent = agents(idx)
      @inline def iterator: Iterator[Agent] = agents.iterator
      @inline def length: Int = agents.length

      /* RHZ: Why is this different to the foreach given by extends Seq?
      // -- Extra Seq[Agent] API --
      @inline override def foreach[U](f: Agent => U): Unit =
        agents foreach f
      */

      // Update the component pointers in the agents
      for ((a, i) <- agents.zipWithIndex) {
        a._component = this
        a._index = i
      }
    }

    /**
     * Factory method creating an empty pattern.
     *
     * @return an empty pattern.
     */
    def apply() = new Pattern(Vector(), Vector(), "")

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
    //
    // RHZ: I'm not sure I understand what you mean by the states should
    // point to the symbol table and not the other way around
    def apply(expr: String) = {
      val ast = parseSiteGraph(expr) match {
        case Success(ast, _) => ast
        case msg => println(msg); println();
                    throw new IllegalArgumentException(
                      "given site graph '" + expr + "' is invalid")
      }

      val agents: Vector[Agent] =
        (for ((AST.Agent(aname, astate, intf), id) <- ast.zipWithIndex)
         yield {
           val anameSym = agentTypeSyms(aname)
           val sites = for (AST.Site(sname, sstate, lnk) <- intf)
                       yield {
                         val siteIndex = siteNameSyms(aname)(sname)
                         val siteState = mkSiteState(
                           siteIndex,
                           sstate map siteStateNameSyms(aname)(sname))

                         (siteIndex, Site(siteState, lnk match {
                           case AST.Stub          => Stub
                           case AST.Undefined     => Undefined
                           case AST.Wildcard      => Wildcard(None, None, None)
                           case AST.Linked(label) => Undefined // should be Linked
                         }))
                       }
           val siteMap = sites.toMap withDefault (sn => Site(mkSiteState(sn, None)))
           val interface = siteNames(anameSym).keys map siteMap toVector
           val agentState = mkAgentState(anameSym, astate map agentStateNameSyms(aname))
           // RHZ: I think we actually don't want a symbol table (as in a finite table)
           // but just a function to create symbols from names
           new Agent(agentState, interface)
         }).toVector

      val lstateMap = (for (AST.LinkAnnot(label, lstate) <- ast)
                       yield (label, lstate)).toMap.lift

      val links = for {
        (AST.Agent(aname, _, intf), aindex) <- ast.zipWithIndex
        AST.Site(sname, _, AST.Linked(label)) <- intf
      } yield (label, aindex, aname, sname)

      val pairs = (links groupBy (_._1) values) map {
        case List((label, aindex1, aname1, sname1), (_, aindex2, aname2, sname2)) =>
          (aindex1, aname1, sname1, aindex2, aname2, sname2, label)
        case _ => throw new IllegalArgumentException("every bond label must appear exactly twice")
      }

      val componentMaker = new ComponentMaker(agents)

      // Connect sites
      // NB What I didn't like about the merge and connect function
      // is that they looked like pure functions but connect had side
      // effects: the original graph would have the same number of
      // connected components as before but the sites are Linked
      //
      // It would be nicer I think if the merge function had side
      // effects too: that it merge the two components in place instead
      // of giving you back a new Pattern. Note that the problem is not
      // really in the merge function but in the connect function: it
      // returns a new Pattern but the original one is not valid anymore.
      // I think both functions should return return `this`.
      pairs foreach {
        case (aindex1, aname1, sname1, aindex2, aname2, sname2, label) => {
          if (! (linkStateNameSyms contains (aname1, sname1, aname2, sname2)))
            throw new IllegalArgumentException(
              "link (" + aname1 + ", " + sname1 + ", " + aname2 + ", " + sname2 + ") is not allowed by contact graph")

          val lstate = lstateMap(label) map linkStateNameSyms((aname1, sname1, aname2, sname2))
          val s1 = agents(aindex1).sites(siteNameSyms(aname1)(sname1))
          val s2 = agents(aindex2).sites(siteNameSyms(aname2)(sname2))
          s1.link = Linked(s1, mkLinkState(lstate))
          // TODO What do we do about the directed link states issue?
          s2.link = Linked(s2, mkLinkState(lstate))

          componentMaker.merge(s1.agent, s2.agent)
        }
      }

      new Pattern(componentMaker.components, agents, expr)
    }

    // RHZ: This class is for creating connected components
    // When you create it every agent is in a different component
    // You can merge two components using `merge`
    // Finally you get the vector of components calling `components`
    private class ComponentMaker(val agents: Vector[Agent])
    {
      var ccs: Map[ComponentIndex, Vector[Agent]] = agents.zipWithIndex map { case (a, i) => (i, Vector(a)) } toMap
      var ccIds: Map[Agent, ComponentIndex] = agents.zipWithIndex.toMap

      def merge(a1: Agent, a2: Agent) {
        val id1 = ccIds(a1)
        val id2 = ccIds(a2)
        val (minId, maxId) = if (id1 < id2) (id1, id2) else (id2, id1)
        ccs(maxId) foreach { agent => ccIds = ccIds updated (agent, minId) }
        ccs = ccs - maxId
      }

      def components(): Vector[Component] = ccs.values map (new Component(_)) toVector
    }
  }

  implicit def stringToPattern(s: String) : Pattern = Pattern(s)
}

