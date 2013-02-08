package kappa

import scala.language.implicitConversions
import scala.language.postfixOps

import scala.collection.mutable

trait Patterns {
  this: LanguageContext with Parser with Actions with Rules with Symbols
      with Mixtures with ComponentEmbeddings with Embeddings =>

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
    def inMix: Double = (components map (_.inMix)).product

    /**
     * Pick one of the embeddings candidates from this pattern to the
     * mixture uniformly at random.
     *
     * NOTE: The embedding returned by this method might contain
     * clashes!
     *
     * @param rand the PRNG used as a source of randomness.
     */
    @inline def randomEmbedding(rand: util.Random) = {
      new Embedding(
        for (c <- components) yield c.randomEmbedding(rand),
        this)
    }

    /** Prune the embedding set of each component of this pattern. */
    def pruneEmbeddings {
      for (c <- components) {
        c.pruneEmbeddings
      }
    }

    /** Register the components of this pattern in the model. */
    def registerComponents = for (c <- components) c.register

    // RHZ: @Sandro Beware that connect has side-effects and therefore
    // siteGraphString won't return the correct string after any of these
    // modifications. An easy fix is to create new Patterns giving
    // an empty string as siteGraphString when connecting or merging.
    // That doesn't solve the problem that the original sites will have
    // their links changed when connecting though.
    override def toString =
      if (!siteGraphString.isEmpty) siteGraphString
      else iterator.mkString("", ",", "")

    /**
     * Construct an action with `this` and `that` as LHS and RHS,
     * respectively.
     */
    def -> (that: Pattern) = Action(this, that)


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
  /** Companion object of the [[Patterns#Pattern]] class. */
  object Pattern {

    /** A class representing links between [[Pattern.Site]]s. */
    sealed abstract class Link extends Matchable[Link]{

      // -- Matchable[Link] API --

      /**
       * Compare this link against another [[Pattern.Link]].
       *
       * @return `true` if `this` matches `that`.
       */
      @inline final
      def matches(that: Link): Boolean = (this, that) match {
        case (Undefined, _) => true
        case (Stub, Stub) => true
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) =>
          (a1 matches a2) && (s1 matches s2) && (l1 matches l2)
        case (Wildcard(a1, s1, l1), Linked(s2, l2)) =>
          (a1 matches Some(s2.agent.state)) &&
          (s1 matches Some(s2.state)) &&
          (l1 matches Some(l2))
        case (Linked(s1, l1), Linked(s2, l2)) =>
          (s1.index == s2.index) && (l1 matches l2)
        case _ => false
      }

      /**
       * Compare this link against a [[Mixtures#Mixture.Link]].
       *
       * @return `true` if `this` matches `that`.
       */
      @inline final
      def matches(that: Mixture.Link): Boolean = (this, that) match {
        case (Undefined, _) => true
        case (Stub, Mixture.Stub) => true
        case (Wildcard(a1, s1, l1), Mixture.Linked(a2, s2, l2)) =>
          (a1 matches Some(a2.state)) &&
          (s1 matches Some(a2.sites(s2).state)) &&
          (l1 matches Some(l2))
        case (Linked(s1, l1), Mixture.Linked(_, s2, l2)) =>
          (s1.index == s2) && (l1 matches l2)
        case _ => false
      }

      /**
       * Returns the join of this link and `that`.
       *
       * FIXME: There is one case that is a little problematic: if we
       * find both `this` and `that` are instances of `Linked`
       * pointing two the same site in two agents `u1`and `u2`, we
       * don't really know what the join of `u1` and `u2` is, and we
       * can't tell whether it even exists, because attempting to
       * compute `u1 join u2` will throw us straight into a recursive
       * loop (the join of `u1` and `u2` does, after all, depend on
       * the join of the links `this` and `that`).  So we'll just
       * assume that `u1` and `u2` do indeed have some join, return
       * the appropriate instance of `Linked` (albeit pointing to `u1`
       * for now), and leave it to whoever wants to traverse the
       * entire site graph and construct a particular cogluing to
       * decide whether they want to back-patch the link.
       *
       * @return the join of `this` and `that`.
       */
      final def join(that: Link): Option[Link] = (this, that) match {
        case (Stub, Stub) => Some(Stub)
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) => {
          val wc =
            for (a <- a1 join a2; s <- s1 join s2; l <- l1 join l2)
            yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (Wildcard(a1, s1, l1), Linked(s2, l2)) => {
          val wc = for {
            a <- a1 join Some(s2.agent.state)
            s <- s1 join Some(s2.state)
            l <- l1 join Some(l2)
          } yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (Linked(s1, l1), Wildcard(a2, s2, l2)) => {
          val wc = for {
            a <- Some(s1.agent.state) join a2
            s <- Some(s1.state)       join s2
            l <- Some(l1)             join l2
          } yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (Linked(s1, l1), Linked(s2, l2)) if (s1.index == s2.index) => {
          val l = l1 join l2
          l map (Linked(s1, _)) orElse {
            val a = s1.agent.state meet s2.agent.state
            val s = s1.state       meet s2.state
            Some(Wildcard(a, s, l))
          }
        }
        case _ => Some(Undefined)
      }

      /**
       * Returns the meet of this link and `that`.
       *
       * FIXME: This method has the same problem as [[Link.join]],
       * except it may return instances of `Linked` even when only one
       * of the links is an instance of `Linked` and the other one is
       * a compatible Wildcard.
       *
       * @return the meet of `this` and `that`.
       */
      final def meet(that: Link): Option[Link] = (this, that) match {
        case (_, Undefined) => Some(this)
        case (Undefined, _) => Some(that)
        case (Stub, Stub)   => Some(Stub)
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) =>
          for (a <- a1 meet a2; s <- s1 meet s2; l <- l1 meet l2)
          yield new Wildcard(a.om, s.om, l.om)
        case (Wildcard(a1, s1, l1), Linked(s2, l2)) =>
          // FIXME: See note above.
          for {
            a <- a1 meet Some(s2.agent.state)
            s <- s1 meet Some(s2.state)
            l <- l1 meet Some(l2)
          } yield new Linked(s2, l.om.get)
        case (Linked(s1, l1), Wildcard(a2, s2, l2)) =>
          // FIXME: See note above.
          for {
            a <- Some(s1.agent.state) meet a2
            s <- Some(s1.state)       meet s2
            l <- Some(l1)                 meet l2
          } yield new Linked(s1, l.om.get)
        case (Linked(s1, l1), Linked(s2, l2)) if (s1.index == s2.index) =>
          // FIXME: See note above.
          l1 meet l2 map (Linked(s1, _))
        case _ => None
      }

      /**
       * Checks if this link can be used in mixtures.
       *
       * @return `true` if this link is valid in a mixture.
       */
      @inline final def isComplete = this match {
        case Stub => true
        case Linked(_, _) => true
        case _ => false
      }

      // -- Any API --
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
    final case class Site(val state: SiteState,
                          var link: Link = Undefined)
        extends Matchable[Site]
    {
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
          "attempt to access index of orphan site")
        else _index

      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @return `Some(x)`, where `x` is the neighboring site of this
       *         site, if this site is connected, and `None` otherwise.
       */
      @inline def neighbour: Option[Site] =
        link match {
          case Linked(s, _) => Some(s)
          case _ => None
        }

      // -- Matchable[Site] API --

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

      /**
       * Returns the join of this site and `that`.
       *
       * @return the join of `this` and `that`.
       */
      final def join(that: Site): Option[Site] = for {
        s <- this.state join that.state
        l <- this.link join that.link
      } yield new Site(s, l)

      /**
       * Returns the meet of this site and `that`.
       *
       * @return the meet of `this` and `that`.
       */
      final def meet(that: Site): Option[Site] = for {
        s <- this.state meet that.state
        l <- this.link meet that.link
      } yield new Site(s, l)

      /**
       * Checks if this site can be used in mixtures.
       *
       * @return `true` if this site is valid in a mixture.
       */
      def isComplete = this.state.isComplete && this.link.isComplete

      // -- Any API --
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

    /**
     * A class representing agents in [[Pattern.Component]]s.
     *
     * @param state the state of the agent.
     * @param sites the sites of the agent.
     */
    case class Agent(val state: AgentState, val sites: Vector[Site])
        extends Matchable[Agent]
    {
      /** The component this agent belongs to. */
      protected[Pattern] var _component: Component = null

      /** The component this agent belongs to. */
      @inline def component =
        if (_component == null) throw new NullPointerException(
          "attempt to access parent component of orphan agent")
        else _component

      /** The index of the agent within the component. */
      protected[Pattern] var _index: AgentIndex = -1

      /** The index of the agent within the component. */
      @inline def index =
        if (_index < 0) throw new NullPointerException(
          "attempt to access index of orphan agent")
        else _index

      /**
       * Returns the neighbor of a site if it is connected.
       * 
       * @param i site index
       * @return `Some(a, j)`, where `a` and `j` are the neighboring agent
       *          and site of this agent at site index i, if this site is
       *          connected, and `None` otherwise.
       */
      @inline def neighbour(i: SiteIndex): Option[(Agent, SiteIndex)] =
        sites(i).link match {
          case Linked(s, _) => Some((s.agent, s.index))
          case _ => None
        }

      // -- Matchable[Agent] API --

      /**
       * Compare this agent against a [[Mixtures#Mixture.Agent]].
       *
       * @return `true` if this agent matches `that`.
       */
      def matches(that: Mixture.Agent): Boolean =
        (that.sites.size == this.sites.size) &&
        (this.state matches that.state) &&
        (this.sites zip that.sites forall { case (s1, s2) => s1 matches s2 })

      /**
       * Compare this agent against another [[Pattern.Agent]].
       *
       * @return `true` if this agent matches `that`.
       */
      def matches(that: Agent): Boolean =
        (this.sites.size == that.sites.size) &&
        (this.state matches that.state) &&
        (this.sites zip that.sites forall { case (s1, s2) => s1 matches s2 })

      /**
       * Returns the join of this agent and `that`.
       *
       * ''WARNING'': Agents returned from this method are
       * orphans, i.e. they don't belong to any component and should
       * be handled with care.  Also, their links might be
       * problematic, see comment at [[Link.join]].
       *
       * @return the join of `this` and `that`.
       */
      final def join(that: Agent): Option[Agent] =
        if (this.sites.size != that.sites.size) None
        else {

          // Generate all "combinations" of site joins
          def intfs = {
            val sitesJoins =
              for (i <- 0 until sites.size)
              yield this.sites(i) join that.sites(i)

            val intfs: Option[Vector[Site]] = Some(Vector())
            sitesJoins.foldLeft(intfs) {
              (intfs, siteJoins) =>
              for (intf <- intfs; j <- siteJoins) yield intf :+ j
            }
          }

          for {
            st <- this.state join that.state
            intf <- intfs
          } yield {
            val u = Agent(st, intf.toVector)
            u._index = this._index
            u
          }
        }

      /**
       * Returns the meet of this agent and `that`.
       *
       * ''WARNING'': Agents returned from this method are
       * orphans, i.e. they don't belong to any component and should
       * be handled with care.  Also, their links might be
       * problematic, see comment at [[Link.meet]].
       *
       * @return the meet of `this` and `that`.
       */
      final def meet(that: Agent): Option[Agent] =
        if (this.sites.size != that.sites.size) None
        else {

          // Generate all "combinations" of site meets
          def intfs = {
            val sitesMeets =
              for (i <- 0 until sites.size)
              yield this.sites(i) meet that.sites(i)

            val intfs: Option[Vector[Site]] = Some(Vector())
            sitesMeets.foldLeft(intfs) {
              (intfs, siteMeets) =>
              for (intf <- intfs; m <- siteMeets) yield intf :+ m
            }
          }

          for {
            st <- this.state meet that.state
            intf <- intfs
          } yield {
            val u = Agent(st, intf.toVector)
            u._index = this._index
            u
          }
        }

      /**
       * Checks if this agent can be used in mixtures.
       *
       * @return `true` if this agent is valid in a mixture.
       */
      def isComplete =
        this.state.isComplete && (this.sites forall (_.isComplete))

      // -- Equals API --
      /** TODO: Is this method redundant? */
      override def canEqual(that: Any) = that.isInstanceOf[Agent]

      // -- Any API --
      /**
       * Tests whether the argument (`that`) is a reference to the
       * receiver object (`this`).
       *
       * NOTE: There are multiple reasons for overriding the `equals`
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
    case class Component(val agents: Vector[Agent])
        extends Seq[Agent] with Matchable[Component]
    {
      /** The pattern this component belongs to. */
      protected[Pattern] var _pattern: Pattern = null

      /** The pattern this component belongs to. */
      def pattern =
        if (_pattern == null) throw new NullPointerException(
          "attempt to access parent pattern of orphan component")
        else _pattern

      /** The index of the component within the pattern. */
      protected[Pattern] var _index: ComponentIndex = -1

      /** The index of the component within the pattern. */
      def index =
        if (_index < 0) throw new NullPointerException(
          "attempt to retrieve in-pattern index of orphan component")
        else _index

      /** The index of the component within the model. */
      protected[Pattern] var _modelIndex: ComponentIndex = -1

      /** The index of the component within the model. */
      def modelIndex: ComponentIndex =
        if (_modelIndex > 0) _modelIndex else register

      /**
       * The component embeddings from this component to the mixture.
       *
       * NOTE: The role of this buffer allow random access to
       * embeddings (literally).  If we just kept embeddings in a map,
       * we could not pick one of the embeddings uniformly at random
       * (in O(1) time).
       *
       * FIXME: Can we do better?
       */
      protected[Pattern] var _embeddings
          : mutable.ArrayBuffer[ComponentEmbedding] = null

      /**
       * The component embeddings from this component to the mixture.
       *
       * NOTE: This is just a convenience method for making sure we're
       * always accessing the correct set of embeddings.
       */
      @inline protected[Pattern] def embeddings = {
        val representative = patternComponents(this.modelIndex)
        representative._embeddings
      }

      /**
       * A hash map from embedding roots to embedding indices.
       *
       * NOTE: The role of this map is dual:
       *
       *  1) quickly find the index of a given embedding in
       *     [[Component.embeddings]];
       *
       *  2) checking uniqueness of embeddings in
       *     [[Component.embeddings]].
       */
      protected[Pattern] var _embeddingIndices
          : mutable.HashMap[Mixture.Agent, EmbeddingIndex] = null

      /**
       * A hash map from embedding roots to embedding indices.
       *
       * NOTE: This is just a convenience method for making sure we're
       * always accessing the correct set of embeddings.
       */
      @inline protected[Pattern] def embeddingIndices = {
        val representative = patternComponents(this.modelIndex)
        representative._embeddingIndices
      }

      /**
       * Return all the embeddings from this pattern component in a
       * given mixture.
       *
       * @return all the component embeddings from `this` in `that`.
       */
      def embeddingsIn(that: Mixture): Seq[ComponentEmbedding] = {
        val u = this.head
        (for (v <- that) yield ComponentEmbedding(u, v)).flatten
      }

      @inline def initEmbeddings {
        this.embeddings.clear
        this.embeddingIndices.clear
        for (ce <- embeddingsIn(mix)) {
          this.addEmbedding(ce)
        }
      }

      /**
       * Add a component embedding to the collection of embeddings
       * from this component to the mixture.
       */
      @inline def addEmbedding(ce: ComponentEmbedding) {
        if (!(embeddingIndices contains ce.head)) {
          val i = embeddings.length
          embeddings += ce
          embeddingIndices += ((ce.head, i))
        }
      }

      /**
       * Remove a component embedding from the collection of
       * embeddings from this component to the mixture.
       */
      @inline def removeEmbedding(ce: ComponentEmbedding) {
        (embeddingIndices remove ce.head) match {
          case Some(i) => {
            // Clear lifts
            for (i <- 0 until ce.length) ce(i).removeLift(ce.component(i), ce)

            // Remove embedding from collection
            val j = embeddings.length - 1
            if (i != j) {
              val ce = embeddings(j)
              embeddings(i) = ce
              embeddingIndices.update(ce.head, i)
            }
            embeddings.reduceToSize(j)
          }
          case None => { }
        }
      }

      /**
       * Return the number of matchings of this pattern component in
       * the target mixture.
       */
      @inline def inMix: Int = embeddings.size

      /**
       * Pick one of the embeddings from this component to the mixture
       * uniformly at random.
       *
       * @param rand the PRNG used as a source of randomness.
       */
      @inline def randomEmbedding(rand: util.Random) = {
        embeddings(rand.nextInt(embeddings.size))
      }

      /**
       * Remove inconsistent embeddings from the embedding set of this
       * pattern component.
       */
      def pruneEmbeddings {
        var garbage = 0
        var k = 0
        while (k < embeddings.length) {
          val ce = embeddings(k)
          val consistent = (0 until agents.length) forall { i =>
            val u = agents(i)
            val v = ce(i)
            (u matches v) && (u.sites.indices forall {
              j =>
              (u.neighbour(j), v.neighbour(j)) match {
                case (None, _) => true
                case (Some((w1, _)), Some((w2, _))) => ce(w1.index) == w2
                case _ => false
              }
            })
          }
          if (!consistent) {
            removeEmbedding(ce)
            garbage += 1
          } else {
            k += 1
          }
        }
        // println("# found and collected " + garbage +
        //   " garbage embeddings for component # " + modelIndex + ".")
      }

      /**
       * If this component does not have a representative already
       * registered in the model, register it.
       *
       * @return the index of the representative of this component
       *         in the model.
       */
      def register: ComponentIndex =
        if (_modelIndex >= 0) _modelIndex else {

          // FIXME: This "isEquivTo" in the following loop is an
          // isomorphism check, so this loop is expensive! One iso
          // check is O(n^2) in the size of the components involved,
          // so the total cost of checking whether there is already a
          // registered representative of this component is O(n^2 * m)
          // with m the number of components.
          //
          // We can do better! If we were to use either Jerome's or
          // Michael's & Nicolas' canonical representations to
          // properly implement a hashCode method in this class, we
          // could store the patterns in a hash map instead and speed
          // this up to O(n^2 + m log m)!
          _modelIndex = patternComponents indexWhere {
            c => c isEquivTo this
          }

          if (_modelIndex < 0) {
            // No representative found.  Register this component as
            // the representative of its isomorphism class.
            _modelIndex = patternComponents.length
            patternComponents += this

            // Allocate the embedding set
            this._embeddings = new mutable.ArrayBuffer[ComponentEmbedding]()
            this._embeddingIndices =
              new mutable.HashMap[Mixture.Agent, EmbeddingIndex]()

            // Initialize the embedding set
            initEmbeddings

            println("# Registered component # " + _modelIndex + ": CC " +
              _index + " of pattern " + _pattern)

            // Let rules now about this component so they can update
            // their positive influence map if necessary.
            for (r <- rules) {
              r.action.addActivation(this)
            }
          }

          _modelIndex
        }


      // -- Matchable[Component] API --

      /**
       * Compare this link against a [[Mixtures#Mixture.Link]].
       *
       * @return `true` if `this` matches `that`.
       */
      @inline def matches(that: Component) =
        !ComponentEmbedding.findEmbedding(this(0), that(0)).isEmpty

      /**
       * Check if this component is equivalent to `that`.
       *
       * @return `true` if and only if `this` and `that` are equivalent.
       */
      // FIXME: Should override this for better performance
      //override def isEquivTo[U <: Component](that: U): Boolean = ???

      /**
       * Returns the join of this component and `that`.
       *
       * NOTE: This method always returns `None`.  The minimal upper
       * bounds of two pattern components are the ''coglueings'' (or
       * ''local products'') of the components, which are not
       * unique in general.  More importantly though, the coglueings
       * of two connected components need not be connected in general.
       * Returning a pattern instead would be a type error.
       *
       * @return `None`.
       */
      def join(that: Component): Option[Component] = None

      /**
       * Returns the meet of this component and `that`.
       *
       * NOTE: For now, this method always returns `None`.  The
       * maximal upper bounds of two pattern components are the
       * ''minimal glueings'' (or ''local coproducts'') of the
       * components, which are not unique in general.  More
       * importantly though, the one minimal glueing that is
       * guaranteed to exist for any two connected components is the
       * disjoint union of the two components.  Obviously, this
       * minimal glueing is not a connected component.  Returning a
       * pattern instead would be a type error.
       *
       * @return `None`.
       */
      def meet(that: Component): Option[Component] = None

      /**
       * Checks if this link can be used in mixtures.
       *
       * @return `true` if this link is valid in a mixture.
       */
      @inline def isComplete = this.agents forall (_.isComplete)

      // -- Core Seq[Agent] API --
      @inline def apply(idx: Int): Agent = agents(idx)
      @inline def iterator: Iterator[Agent] = agents.iterator
      @inline def length: Int = agents.length

      /* RHZ: Why is this different to the foreach given by Seq?
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
     * Factory method creating a pattern from a string.
     *
     * This factory method invokes the [[Parser]] to parse a Kappa
     * expression.  It then walks the [[Parser.AST]] and builds a
     * [[Patterns.Pattern]] from the expression.
     *
     * @return a pattern corresponding to the expression `expr`.
     */
    def apply(expr: String): Pattern = {
      val ast = parseSiteGraph(expr)
      val lstates = for (AST.LinkAnnot(bondLabel, lstate) <- ast)
                    yield (bondLabel, lstate)
      val pm = new PatternBuilder(expr, lstates.toMap.lift)

      for (AST.Agent(atype, astate, intf) <- ast)
        pm.add(atype, astate, intf)

      pm.build
    }

    /**
     * Builder for patterns. Not as elegant yet but maybe better than having
     * everything in the Pattern.apply method
     *
     * @param siteGraphString the string representing the pattern to build
     */
    private class PatternBuilder(val siteGraphString: String,
                                 val lstateMap: AST.BondLabel => Option[LinkStateName])
    {
      val componentBuilder: ComponentBuilder = new ComponentBuilder()
      var agents: Vector[Agent] = Vector()
      var pairs: Map[AST.BondLabel, List[(AgentType, SiteName, Site)]] = Map() withDefaultValue List()
      //var pairs: Map[AST.BondLabel, List[Site]] = Map() withDefaultValue List()

      def add(atype: AgentType, astate: Option[AgentStateName], intf: Seq[AST.Site]) {
        val sites = for (AST.Site(sname, sstate, lnk) <- intf)
                    yield {
                      val site = Site(mkSiteState(atype, sname, sstate),
                                      lnk match {
                        case AST.Stub       => Stub
                        case AST.Undefined  => Undefined
                        case AST.Wildcard   => Wildcard(None, None, None)
                        case AST.Linked(_)  => Undefined
                      })

                      lnk match {
                        case AST.Linked(bondLabel) =>
                          pairs += ((bondLabel, (atype, sname, site) :: pairs(bondLabel)))
                        //pairs += ((bondLabel, site :: pairs(bondLabel)))
                        case _ => ()
                      }

                      (sname, site)
                    }

        def undefinedSite(sname: SiteName) =
          Site(mkSiteState(atype, sname, None), Undefined)

        val interface = siteNames(atype) map (sites.toMap withDefault undefinedSite)

        val agent = new Agent(mkAgentState(atype, astate), interface)

        componentBuilder add agent
        agents = agents :+ agent
      }

      def connect(s1: Site, lstate1: LinkState,
                  s2: Site, lstate2: LinkState) {
        // TODO check if link is allowed by contact graph
        s1.link = Linked(s2, lstate1)
        s2.link = Linked(s1, lstate2)
        componentBuilder.merge(s1.agent, s2.agent)
      }

      def build: Pattern = {
        pairs foreach {
          case (bondLabel, List((atype2, sname2, s2), (atype1, sname1, s1))) => {
            val link = (atype1, sname1, atype2, sname2)
            val lstate1 = mkLinkState(link, lstateMap(bondLabel))
            val lstate2 = mkLinkState(link, lstateMap(bondLabel) map invertLinkStateName)
            connect(s1, lstate1, s2, lstate2)
          }
          /*
          case (bondLabel, List(s1, s2)) => {
            val lstate = mkLinkState(lstateMap(bondLabel))
            connect(s1, lstate, s2, lstate)
          }
          */
          case _ => throw new IllegalArgumentException(
            "every bond label must appear exactly twice")
        }
        new Pattern(componentBuilder.build, agents, siteGraphString)
      }

      // RHZ: This class is for creating connected components
      // When you create it every agent is in a different component
      // You can merge two components using `merge`
      // Finally you get the vector of components calling `components`
      // 
      // RHZ: Perhaps I should just use the agents vector in PatternBuilder?
      class ComponentBuilder(var agents: Vector[Agent] = Vector())
      {
        var ccs: Map[ComponentIndex, Vector[Agent]] =
          agents.zipWithIndex map { case (a, i) => (i, Vector(a)) } toMap
        var ccIds: Map[Agent, ComponentIndex] = agents.zipWithIndex.toMap

        def add(a: Agent) {
          agents = agents :+ a
          ccs += ((agents.length, Vector(a)))
          ccIds += ((a, agents.length))
        }

        def merge(a1: Agent, a2: Agent) {
          val id1 = ccIds(a1)
          val id2 = ccIds(a2)
          val (minId, maxId) = if (id1 < id2) (id1, id2) else (id2, id1)

          for (agent <- ccs(maxId))
            ccIds = ccIds updated (agent, minId)

          ccs = ccs.updated(minId, ccs(minId) ++ ccs(maxId)) - maxId
        }

        def build: Vector[Component] = ccs.values map (new Component(_)) toVector
      }
    }
  }

  implicit def stringToPattern(s: String): Pattern = Pattern(s)

  /**
   * The collection of pattern components to track in this model.
   *
   * Actually, this list holds representatives of isomorphism classes
   * of pattern components.  This reduces the total number of
   * embeddings to track as we only ever track one representative of
   * each isomorphism-class of embeddings.
   */
  val patternComponents = new mutable.ArrayBuffer[Pattern.Component]()
}

trait KaSpacePatterns extends Patterns {
  this: KaSpaceContext with KaSpaceParser with Actions with Rules with KaSpaceSymbols
      with Mixtures with ComponentEmbeddings with Embeddings =>

  implicit def scToPattern(sc: StringContext): ToPattern = new ToPattern(sc)

  class ToPattern(sc: StringContext) {
    def p(args: Any*): Pattern = {
      def getString[T](x: T): String = x match {
        case x: Double => x.toString
        case xs: Vector[_] => "[" + xs.map(getString).mkString(", ") + "]"
      }

      val argStrings = for (arg <- args) yield getString(arg)
      Pattern( sc.s(argStrings :_*) )
    }
  }
}
