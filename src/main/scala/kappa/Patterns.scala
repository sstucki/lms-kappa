package kappa

import scala.collection.mutable


trait Patterns {
  this: LanguageContext
      with ContactGraphs
      with SiteGraphs
      with Mixtures
      with Actions
      with Rules
      with ComponentEmbeddings
      with Embeddings
      with AbstractSyntax =>

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
    val components: Array[Pattern.Component],
    val agents: Array[Pattern.Agent])
      extends Seq[Pattern.Agent] {

    import SiteGraph.{Link, Undefined, Stub, Wildcard}
    import Pattern._

    /**
     * Return the (overestimated) number of matchings of this pattern
     * in the target mixture.
     */
    def inMix: Int = (components map (_.inMix)).product

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
      new Embedding[Mixture.Agent](
        for (c <- components) yield c.randomEmbedding(rand), this)
    }

    /** Prune the embedding set of each component of this pattern. */
    def pruneEmbeddings {
      for (c <- components) {
        c.pruneEmbeddings
      }
    }

    /** Register the components of this pattern in the model. */
    def registerComponents: Seq[ComponentIndex] =
      for (c <- components) yield c.register

    override def toString = iterator.mkString("", ",", "")

    /** Converts pattern into a mixture. */
    def toMixture: Mixture = {

      val m = new Mixture
      for (c <- this.components) {

        if (!c.isComplete) throw new IllegalArgumentException(
          "attempt to create mixture from incomplete pattern: " + this)

        // Allocate unconnected copies of agents in this component
        val as = new Array[Mixture.Agent](c.agents.size)
        for (u <- c.agents) {
          val n = u.length
          // TODO: Are the siteStates shared or copied?
          val v = new Mixture.AgentImpl(u.agentType, u.state,
            u.siteTypes.toArray, u.siteStates.toArray,
            new Array[Link](n))
          // u.siteStates.copyToArray(v._siteStates)
          m += v
          as(u.index) = v
        }

        // Setup the interfaces of the agents in the component
        for (u <- c.agents) {
          for (i <- u.indices) {
            val l = u.links(i) match {
              case Linked(u, j, t, l) =>
                Mixture.Linked(as(u.index), j, t, l)
              case Stub => Stub
              case _ => throw new IllegalArgumentException(
                "attempt to create mixture with an undefined or " +
                  "wildcard link")
            }
            as(u.index).links(i) = l
          }
        }
      }
      m
    }


    // -- BiAction construction --

    /** Bidirectional action (BiAction) construction. */
    def <->(that: Pattern)(implicit bab: BiActionBuilder)
        : bab.BiRuleBuilder = bab(this, that)

    /** Bidirectional action (BiAction) construction. */
    def <->(that: AbstractPattern)(implicit bab: BiActionBuilder)
        : bab.BiRuleBuilder = bab(this, that.toPattern)

    def <->(that: AbstractBiRuleTail)(implicit bab: BiActionBuilder) = {
      val rhs = that.rhs.toPattern
      BiRule(bab(this, rhs).getBiAction, that.toFwdRate(this),
        that.toBwdRate(rhs))
    }

    /** Action construction. */
    def ->(that: Pattern)(implicit ab: ActionBuilder)
        : ab.RuleBuilder = ab(this, that)

    /** Action construction. */
    def ->(that: AbstractPattern)(implicit ab: ActionBuilder)
        : ab.RuleBuilder = ab(this, that.toPattern)

    def ->(that: AbstractRuleTail)(implicit ab: ActionBuilder) =
      Rule(ab(this, that.rhs.toPattern).getAction, that.toRate(this))


    // -- Core Seq[Pattern.Agent] API --
    @inline def apply(idx: Int): Pattern.Agent = agents(idx)
    @inline def iterator: Iterator[Pattern.Agent] = agents.iterator
    @inline def length: Int = agents.length

    @inline def apply(ci: ComponentIndex, ai: AgentIndex): Pattern.Agent =
      components(ci).agents(ai)
  }


  /** Companion object of the [[Patterns#Pattern]] class. */
  object Pattern extends SiteGraph {

    import SiteGraph.{AgentType, SiteType, LinkType,
      Link, Undefined, Stub, Wildcard}

    // TODO: Maybe I should put agentType inside state instead
    /**
     * A class representing agents in [[Pattern.Component]]s.
     *
     * @param state the state of the agent.
     * @param siteStates the states of the sites of the agent.
     * @param links the links of the sites of the agent.
     */
    final class Agent(
      val agentType: AgentType,
      val state: AgentState,
      val siteTypes: Array[SiteType],
      val siteStates: Array[SiteState],
      val links: Array[Link]) extends AgentIntf {

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


      // -- Pattern.AgentIntf API --

      /** The number of sites of this agent. */
      @inline override def length: Int = links.length

      /** The range of indices of this agent. */
      @inline override def indices: Range = 0 until links.length
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
    case class Component private[Pattern] (val agents: Array[Agent])
        extends Seq[Agent] with Matchable[Component] {

      /** The pattern this component belongs to. */
      protected[Pattern] var _pattern: Pattern = null

      /** The pattern this component belongs to. */
      @inline def pattern =
        if (_pattern == null) throw new NullPointerException(
          "attempt to access parent pattern of orphan component")
        else _pattern

      /** The index of the component within the pattern. */
      protected[Pattern] var _index: ComponentIndex = -1

      /** The index of the component within the pattern. */
      @inline def index =
        if (_index < 0) throw new NullPointerException(
          "attempt to retrieve in-pattern index of orphan component")
        else _index

      /** The index of the component within the model. */
      protected[Pattern] var _modelIndex: ComponentIndex = -1

      /** The index of the component within the model. */
      @inline def modelIndex: ComponentIndex =
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
      protected[Pattern] var _embeddings: mutable.ArrayBuffer[
        ComponentEmbedding[Mixture.Agent]] = null

      /**
       * The component embeddings from this component to the mixture.
       *
       * NOTE: This is just a convenience method for making sure we're
       * always accessing the correct set of embeddings.
       */
      @inline /*protected[Pattern]*/ def embeddings = {
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
      def embeddingsIn(that: Mixture)
          : Seq[ComponentEmbedding[Mixture.Agent]] = {
        val u = this.head
        val ceOpts =
          for (v <- that) yield ComponentEmbedding.findEmbedding(u, v)
        ceOpts.flatten
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
      @inline def addEmbedding(ce: ComponentEmbedding[Mixture.Agent]) {
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
      @inline def removeEmbedding(ce: ComponentEmbedding[Mixture.Agent]) {
        (embeddingIndices remove ce.head) match {
          case Some(i) => {
            // Clear lifts
            for (k <- 0 until ce.length) {
              ce(k).removeLift(ce.component(k))
            }

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

      // RHZ: This is also for the lazy negative update right?
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
            (u matches v) && (u.indices forall {
              j =>
              (u.neighbour(j), v.neighbour(j)) match {
                case (None, _) => true
                case (Some(w1), Some(w2)) => ce(w1.index) == w2
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

          // TODO: The "isEquivTo" in the following loop is an
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
            this._embeddings =
              new mutable.ArrayBuffer[ComponentEmbedding[Mixture.Agent]]()
            this._embeddingIndices =
              new mutable.HashMap[Mixture.Agent, EmbeddingIndex]()

            // Initialize the embedding set
            // initEmbeddings
            // Embeddings are initialised in Model.initialise

            println("# Registered component # " + _modelIndex + ": CC " +
              _index + " of pattern " + _pattern)

            // Let rules now about this component so they can update
            // their positive influence map if necessary.
            for (r <- rules) {
              r.action.addActivation(this)
            }
          } else println("# Found ISO: " + this + " ~= " +
            patternComponents(_modelIndex) + " (CC #" + _modelIndex + ")")

          _modelIndex
        }


      // -- Matchable[Component] API --

      /**
       * Compare this component against `that`.
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
      // FIXME: We should override this for better performance
      // override def isEquivTo[U <: Component](that: U): Boolean = ???

      /**
       * Returns the join of this component and `that`.
       *
       * NOTE: This method always returns `None`.  The minimal upper
       * bounds of two pattern components are the ''local products''
       * of the components, which are not unique in general.  More
       * importantly though, the local products of two connected
       * components need not be connected in general.  Returning a
       * pattern (instead of a [[Pattern.Component]]) would be a type
       * error.
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
       * pattern (instead of a [[Pattern.Component]] would be a type
       * error.
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

      // RHZ: Why is this different to the foreach given by Seq?
      //
      // sstucki: The default implementation is inherited from
      // Iterable (IIRC) and hence might be less efficient as it needs
      // to create an iterator.  Here we're just delegating to the
      // implementation of the underlying `agents` collection, which
      // should be the most efficient for that particular collection.

      // -- Extra Seq[Agent] API --
      @inline override def foreach[U](f: Agent => U): Unit =
        agents foreach f
    }


    /** Builder for patterns. */
    class Builder() {

      import SiteGraph._

      final class Agent(
        val index: AgentIndex,
        val agentType: AgentType,
        val state: AgentState) {

        final class Site private[Agent] (
          val index: SiteIndex,
          val siteType: SiteType,
          val state: SiteState) {

          @inline def agent = Agent.this

          private var _link: Link = Undefined
          @inline def link = _link

          def define(link: Defined) = {
            _link = _link match {
              case Undefined => link
              case _ => throw new IllegalStateException(
                "attempt to redefine a site previously defined as " +
                _link + " in agent '" + agent.state + "' and site '" +
                this.state + "'")
            }
            this
          }

          // TODO: I should probably use Agent and SiteIndex instead... Why?
          def connect(to: Agent#Site,
            fromType: LinkType, fromState: LinkState,
            toType: LinkType, toState: LinkState) = {
            this define Linked(to, fromType, fromState)
            to   define Linked(this, toType, toState)
            this
          }

          override def toString = state + linkDelim + link
        }

        val sites = mutable.ArrayBuffer[Site]()

        def +=(siteType: SiteType, siteState: SiteState): Site = {
          val s = new Site(sites.length, siteType, siteState)
          sites += s
          s
        }

        def completeInterface: Agent = {
          val intf = agentType.completeInterface((
            for (s <- sites)
            yield (s.siteType, (s.state, s.link))).toMap)
          sites.clear()
          for (((siteType, (state, link)), i) <- intf.zipWithIndex) {
            sites += new Site(i, siteType, state)
            link match {
              case  Undefined => ()
              case l: Defined => sites.last define l
            }
          }
          this
        }

        override def toString = state + "(" + sites + ")"
      }

      // TODO: Get rid of this and use SiteGraph.Linked instead
      final case class Linked(to: Agent#Site,
        linkType: LinkType, state: LinkState)
          extends Defined {
        override def toString = state.toString
      }

      /** The set of agents added to the builder. */
      val agents = mutable.ArrayBuffer[Agent]()

      /** Add a new agent to the builder.
        *
        * @param state the state of the new agent.
        * @return a reference to the new agent.
        */
      def += (agentType: AgentType, state: AgentState) = {
        val u = new Agent(agents.length, agentType, state)
        agents += u
        u
      }

      /** Build the pattern. */
      def build: Pattern = {

        val n = agents.length
        val componentMap: Array[ComponentIndex] = Array.fill(n)(-1)

        def traverseComponent(ci: ComponentIndex, ai: AgentIndex) {
          val todo = new mutable.Queue[AgentIndex]()
          todo.enqueue(ai)
          while (todo.nonEmpty) {
            val i = todo.dequeue
            if (componentMap(i) < 0) {
              componentMap(i) = ci
              val u = agents(i)
              for (s <- u.sites) s.link match {
                case Linked(to, _, _) => todo.enqueue(to.agent.index)
                case _ => ()
              }
            }
          }
        }

        // Build the component map
        var m = 0
        for (i <- 0 until n) {
          if (componentMap(i) < 0) {
            traverseComponent(m, i)
            m += 1
          }
        }

        // Allocate the pattern
        val as = new Array[Pattern.Agent](n)
        val cs = new Array[Pattern.Component](m)
        val p = new Pattern(cs, as)

        // Collect the agent indices in per-component buffers
        val compAgentIndices =
          Array.fill(m)(new mutable.ArrayBuffer[AgentIndex]())
        val agentCompIndices = new Array[AgentIndex](n)
        for (i <- agents.indices) {
          val cb = compAgentIndices(componentMap(i))
          agentCompIndices(i) = cb.length
          cb += i
        }

        // Allocate components
        for (i <- cs.indices) yield {
          val ca = new Array[Pattern.Agent](compAgentIndices(i).length)
          val c = new Component(ca)
          c._pattern = p
          c._index = i
          cs(i) = c
        }

        // Allocate agents and initialize the components
        // and agents arrays in `p`
        for (i <- as.indices) {

          // Allocate sites and agent
          val u = agents(i).completeInterface
          val n = u.sites.length
          val st = (for (s <- u.sites) yield s.siteType).to[Array]
          val ss = (for (s <- u.sites) yield s.state).to[Array]
          val ls = new Array[Link](n)
          val v = new Pattern.Agent(u.agentType, u.state, st, ss, ls)
          as(i) = v

          // Add the agent to its component
          val c = cs(componentMap(i))
          v._component = c
          v._index = agentCompIndices(i)
          c.agents(v._index) = v
        }

        // Connect sites
        for (i <- agents.indices) {
          val u = agents(i)
          val v = as(i)
          for (j <- u.sites.indices) {
            v.links(j) = u.sites(j).link match {
              // case Undefined         => SiteGraph.Undefined
              // case Stub              => SiteGraph.Stub
              // case Wildcard(a, s, l) => SiteGraph.Wildcard(a, s, l)
              case Linked(to, linkType, state) =>
                Pattern.Linked(as(to.agent.index), to.index,
                  linkType, state)
              case l => l
            }
          }
        }

        p
      }
    }
  }

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

