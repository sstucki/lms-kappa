package kappa

trait Patterns {
  this: LanguageContext with Parser with Actions with Symbols with Mixtures
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
   * @param siteGraphString the string associated with this pattern
   */
  final class Pattern private (
    val components: Vector[Pattern.Component],
    val agents: Vector[Pattern.Agent],
    val siteGraphString: String)
      extends Seq[Pattern.Agent] {

    import Pattern._

    /**
     * Return the (overestimated) number of matchings of this pattern
     * in the target mixture
     */
    def count: Double = (components map (_.count)).product


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

    /**
     * Construct and action with `this` and `that` as LHS and RHS,
     * respectively.
     */
    def -> (that: Pattern) = Action(this, that)


    // -- Core Seq[Agent] API --
    @inline def apply(idx: Int): Agent = agents(idx)
    @inline def iterator: Iterator[Agent] = agents.iterator
    @inline def length: Int = agents.length

    @inline def apply(ci: ComponentIndex, ai: AgentIndex): Agent =
      components(ci).agents(ai)


    /* RHZ: I think all of these are methods we really don't need for simulation
     * that we can safely skip for now
     *
     * sstucki: These are used in test cases because we don't have a
     * parser right now.  Without them the test breaks. :-( I believe
     * you will also find these handy when you implement the parser.
     * As I understood it you build patterns from the AST by first
     * adding all the agents to a pattern in a first traversal and
     * then connecting them in a second pass.  That's exactly what
     * these methods are intended for.
     */

    // -- Extra Seq[Agent] API --

    /**
     * Return a new pattern extending this pattern with an additional
     * (unconnected) agent.
     *
     * @param elem the agent to add.
     * @return a new pattern extending this pattern with an additional
     *         (unconnected) agent.
     */
    @inline def :+(elem: Agent): Pattern = new Pattern(
      components :+ new Component(Vector(elem)),
      agents :+ elem, siteGraphString)

    /**
     * Create a single unconnected agent and return a new pattern
     * extending this pattern with the newly created agent.
     *
     * @param state the state of the agent to add.
     * @param sites the sites of the agent to add.
     * @return a new pattern extending this pattern with an additional
     *         (unconnected) agent.
     */
    @inline def :+(state: AgentState, sites: Site*): Pattern =
      this :+ Agent(state, sites.toArray)

    @inline override def foreach[U](f: Agent => U): Unit =
      components foreach { c => c.agents foreach f }


    // RHZ: Is this a Scala design pattern?
    //
    // sstucki: Not that I know of.  The reason I put this in a
    // separate method is that I used a mutable variable in there and
    // I didn't want that to become part of the class.  Maybe it would
    // have been sufficient to just put it in a separate scope.
    // Anyway, I changed this to a for comprehension now to make it
    // more readable (since it's not performance critical anyway).

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
    sealed abstract class Link extends Matchable[Link]{

      /**
       * Compare this link against another [[Pattern.Link]].
       *
       * @return `true` if `this` matches `that`.
       */
      def matches(that: Link): Boolean = (this, that) match {
        case (Undefined, _) => true
        case (Stub, Stub) => true
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) =>
          (a1 matches a2) && (s1 matches s2) && (l1 matches l2)
        case (Wildcard(a1, s1, l1), Linked(a2, s2, l2)) =>
          (a1 matches Some(a2.state)) &&
          (s1 matches Some(a2.sites(s2).state)) &&
          (l1 matches Some(l2))
        case (Linked(_, s1, l1), Linked(_, s2, l2)) =>
          (s1 == s2) && (l1 matches l2)
        case _ => false
      }

      /**
       * Compare this link against a [[Mixtures#Mixture.Link]].
       *
       * @return `true` if `this` matches `that`.
       */
      def matches(that: Mixture.Link): Boolean = (this, that) match {
        case (Undefined, _) => true
        case (Stub, Mixture.Stub) => true
        case (Wildcard(a1, s1, l1), Mixture.Linked(a2, s2, l2)) =>
          (a1 matches Some(a2.state)) &&
          (s1 matches Some(a2.sites(s2).state)) &&
          (l1 matches Some(l2))
        case (Linked(_, _, l1), Mixture.Linked(_, _, l2)) =>
          l1 matches l2
        case _ => false
      }

      /**
       * Checks if this link can be used in mixtures.
       *
       * @return `true` if this link is valid in a mixture.
       */
      def isComplete = this match {
        case _: Linked => true
        case _ => false
      }

      // FIXME!
      override def toString = this match {
        case Undefined => "?"
        case Wildcard(a, s, l) =>
          "!" + (a getOrElse "_") + "." + (s getOrElse "_") + "." + (l getOrElse "_")
        case Stub => ""
        case Linked(a, s, l) =>
          "!" + a.state + "." + a.sites(s).state + "." + l
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
     * As in [[Mixtures#Mixture.Link]], the target sites are stored in
     * a "relative" fashion, i.e. as (agent, site index) pairs rather
     * than just a reference to the target site.  This simplifies the
     * process of matching patterns to mixtures (i.e. the process of
     * extending pairs of patter and mixture agents into partial
     * embeddings).  For more details, see the documentation of the
     * [[Mixtures#Mixture.Link]] class.
     *
     * @param agent the target agent of this link.
     * @param site the index of the target site in the target agent of
     *        this link.
     * @param state the state of the link from source to target.
     */
    final case class Linked(agent:Agent, site: SiteIndex, state: LinkState)
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
    final case class Site private (
      val state: SiteState, var link: Link = Undefined) extends Matchable[Site] {

      // TODO: this is not used at the moment.  Remove?

      /** Internal reference to the agent this site belongs to. */
      protected[Pattern] var _agent: Agent = null

      // RHZ: I prefer this exception rather than a null pointer exception
      //
      // sstucki: Fair enough.  I assume what you really care about is
      // the message in the exception rather than the type of the
      // exception?  This can still be done without using the overhead
      // of an Option.  The pointer should remain invisible form most
      // purposes anyway.  I changed it accordingly.

      /** The agent this site belongs to. */
      @inline def agent =
        if (_agent == null) throw new NullPointerException(
          "attempt to access parent agent of orphan site")
        else _agent

      /** Convenience copy method. */
      def copy(state: SiteState = this.state, link: Link = this.link) =
        new Site(state, link match {
          case l: Linked => throw new IllegalArgumentException(
            "attempt to copy a linked site!") // don't duplicate links!
          case l => l
        })

      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @return `Some(x)`, where `x` is the neighboring site of this
       *         site, if this site is connected, and `None` otherwise.
       */
      @inline def neighbour: Option[Site] =
        link match {
          case Linked(a, s, _) => Some(a.sites(s))
          case _ => None
        }

      /**
       * Compare this site against another [[Pattern.Site]].
       *
       * @return `true` if `this` matches `that`.
       */
      def matches(that: Site): Boolean =
        (this.state matches that.state) && (this.link matches that.link)

      /**
       * Compare this site against a [[Mixtures#Mixture.Site]].
       *
       * @return `true` if this site matches `that`.
       */
      def matches(that: Mixture.Site): Boolean =
        (this.state matches that.state) && (this.link matches that.link)

      override def toString = state.toString + link

      /**
       * Checks if this site can be used in mixtures.
       *
       * @return `true` if this site is valid in a mixture.
       */
      def isComplete = this.state.isComplete && this.link.isComplete
    }

    // RHZ: Why not as part of the class?
    //
    // sstucki: because factory methods and custom extractors are
    // defined in companion objects in Scala.  See
    // http://daily-scala.blogspot.ch/2009/09/factory-methods-and-companion-objects.html
    // and http://www.scala-lang.org/node/112

    /** Companion object of [[Site]] containing factory methods */
    object Site {
      def apply(state: SiteState) = new Site(state, Undefined)
      def apply(state: SiteState, stub: Boolean) =
        new Site(state, if (stub) Stub else Undefined)
      def apply(state: SiteState, wildcard: Wildcard) = new Site(state, wildcard)
    }

    /**
     * A class representing agents in [[Pattern.Component]]s.
     *
     * ''WARNING'': For convenience, this class provides the interface
     * of a `Seq[Pattern.Site]`.  However, using some methods from the
     * `Seq` API might not result in the expected behavior.  E.g. `++`
     * will return a `Seq[Pattern.Site]` rather than the expected
     * `Pattern.Agent`.
     *
     * @param state the state of the agent.
     * @param sites the sites of the agent.
     */
    case class Agent(val state: AgentState, val sites: Array[Site])
        extends Seq[Site] with Matchable[Agent] {

      // RHZ: made this type-safe
      //
      // sstucki: No, you made it into an Option, that is no more (or
      // less) type-safe than a (null) pointer, it's just more
      // expressive. ;-) But since this pointer is not (supposed to
      // be) used in any interface, this expressiveness isn't really
      // useful.  I reverted it and made the pointer protected.  Now
      // expressive error messages on the other hand are very useful,
      // so I kept that.
      /** The component this agent belongs to. */
      protected[Pattern] var _component: Component = null

      /** The component this agent belongs to. */
      @inline def component =
        if (_component == null) throw new NullPointerException(
          "attempt to access parent component of orphan agent")
        else _component

      // RHZ: Should we make this an Option[AgentIndex] to have a
      // type-safe behaviour?
      //
      // sstucki: No! Using an option does not make it type-safe.  But
      // we ought to protect it.  Fixed this.
      /** The index of the agent within the component. */
      protected[Pattern] var _index: AgentIndex = -1

      /** The index of the agent within the component. */
      @inline def index =
        if (_index < 0) throw new NullPointerException(
          "attempt to  parent component of orphan agent")
        else _index

      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @param site the index of the site whose neighbor we try to find.
       * @return `Some(a, s)`, where `a` is the neighboring agent of
       *         this site and `s` is the index of the neighboring site
       *         in `a`, or `None` if this site is not connected.
       */
      @inline def neighbour(site: SiteIndex): Option[(Agent, SiteIndex)] =
        sites(site).link match {
          case Linked(a, s, _) => Some((a, s))
          case _ => None
        }

      /**
       * Compare this agent against a [[Mixtures#Mixture.Agent]].
       *
       * @return `true` if this agent matches `that`.
       */
      def matches(that: Mixture.Agent): Boolean =
        that.sites.size == this.sites.size &&
        (this.state matches that.state) &&
        (this.sites zip that.sites forall { case (s1, s2) => s1 matches s2 })

      /**
       * Compare this agent against another [[Pattern.Agent]].
       *
       * @return `true` if this agent matches `that`.
       */
      def matches(that: Agent): Boolean =
        that.sites.size == this.sites.size &&
        (this.state matches that.state) &&
        (this.sites zip that.sites forall { case (s1, s2) => s1 matches s2 })

      /**
       * Checks if this agent can be used in mixtures.
       *
       * @return `true` if this agent is valid in a mixture.
       */
      def isComplete =
        this.state.isComplete && (this.sites forall (_.isComplete))

      // -- Core Seq[Site] API --
      @inline def apply(idx: Int): Site = sites(idx)
      @inline def iterator: Iterator[Site] = sites.iterator
      @inline def length: Int = sites.length

      // -- Extra Seq[Site] API --
      @inline override def foreach[U](f: Site => U): Unit =
        sites foreach f

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

      // Register sites.
      //for (s <- sites) s.agent = this
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
    case class Component(val agents: Vector[Agent]) extends Seq[Agent] {

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
       * the target mixture
       */
      // FIXME: implement!
      def count: Int = 1

      /**
       * Return all the (partial) embeddings from this pattern
       * component in a given mixture.
       *
       * @return all the partial embeddings from `this` in `that`.
       */
      def partialEmbeddingsIn(that: Mixture): Seq[PartialEmbedding] =
        (for (u <- this; v <- that) yield PartialEmbedding(u, v)).flatten


      // -- Core Seq[Agent] API --
      @inline def apply(idx: Int): Agent = agents(idx)
      @inline def iterator: Iterator[Agent] = agents.iterator
      @inline def length: Int = agents.length

      // -- Extra Seq[Agent] API --
      @inline override def foreach[U](f: Agent => U): Unit =
        agents foreach f


      // Update the component pointers in the agents.
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
