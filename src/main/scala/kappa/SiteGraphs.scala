package kappa

import collection.mutable


trait SiteGraphs {
  this: LanguageContext
      with ContactGraphs =>

  trait SiteGraph {

    import SiteGraph._

    /** An abstract type representing agents of a site graph. */
    type Agent <: AgentIntf

    /**
     * A trait defining the minimal interface of agents in site
     * graphs.
     *
     * TODO: The `_siteStates` and `_links` methods in this trait
     * force concrete agent classes to provide array-based
     * implementations (or at least accessors) of the respective
     * collections.  Does it make sense to have these methods in this
     * trait or should these be entirely delegated to the implementing
     * class?  Is there a performance benefit to keeping these
     * methods?
     */
    trait AgentIntf
        extends Matchable[SiteGraph#AgentIntf]
           with Equals {

      type T <: ContactGraph.Agent

      // -- Abstract interface --

      /** The type of this agent. */
      val agentType: T

      /** The state of this agent. */
      def state: AgentState

      /** The types of the sites of this agent. */
      // def siteTypes: IndexedSeq[agentType.Site]
      def siteTypes: Array[agentType.Site]

      /** The states of the sites of this agent. */
      // def siteStates: IndexedSeq[SiteState]
      def siteStates: Array[SiteState]

      /** The links of the sites of this agent. */
      // def links: IndexedSeq[Link]
      def links: Array[Link]


      // -- Concrete interface --

      /** The number of sites of this agent. */
      @inline def length: Int = links.length

      /** The range of indices of this agent. */
      @inline def indices: Range = links.indices

      /**
       * Returns the neighbor of a given site of this agent if it is
       * connected.
       *
       * @param i site index
       * @return `Some(u)`, where `u` is the neighboring agent of this
       *         agent at site index `i`, if the site `i` is connected,
       *         and `None` otherwise.
       */
      @inline def neighbour(i: SiteIndex): Option[Agent] =
        links(i) match {
          case Linked(u, j, _, _) => Some(u)
          case _ => None
        }

      /**
       * Returns the neighboring site of a given site of this agent if
       * it is connected.
       *
       * @param i site index
       * @return `Some((u, j))`, where `u` and `j` are the neighboring
       *         agent and site index of this agent at site index `i`,
       *         if the site `i` is connected, and `None` otherwise.
       */
      @inline def neighbourSite(i: SiteIndex)
          : Option[(Agent, SiteIndex)] =
        links(i) match {
          case Linked(u, j, _, _) => Some((u, j))
          case _ => None
        }


      // -- Sites interface --

      /**
       * A class representing sites in [[Agent]]s.
       *
       * @param index the index of this site within the enclosing agent.
       * @param state the state of this site
       * @param link whether and how this site is linked to another site.
       */
      final class SiteIntf private[AgentIntf] (
        val index: SiteIndex, val siteType: agentType.Site,
        val state: SiteState, val link: Link) {

        /** The enclosing agent of this site. */
        @inline def agent = AgentIntf.this

        /**
         * Returns the neighbor of a site if it is connected.
         *
         * @return `Some((u, j))`, where `u` and `j` are the neighboring
         *          agent and site index of this site if it is
         *          connected, and `None` otherwise.
         */
        @inline def neighbour: Option[Agent#SiteIntf] =
          link match {
            case Linked(u, j, _, _) => Some(u.site(j))
            case _ => None
          }

        override def toString = state.toString + link
      }

      // TODO: Are the methods below any useful?
      /**
       * Return a given site of this agent as a [[Agent#SiteIntf]].
       *
       * @param i the index of the site to return.
       * @return the `i`th site of this agent.
       */
      @inline def site(i: SiteIndex): SiteIntf =
        new SiteIntf(i, siteTypes(i), siteStates(i), links(i))

      /**
       * Return a view of the sites of this agent.
       *
       * NOTE: This will return a collection of [[this.SiteIntf]]s
       * representing the sites of this agent.  This method is
       * provided for convenience only, don't use it in performance
       * critical code.
       *
       * @return a view of the sites of this agent.
       */
      @inline def sites: IndexedSeq[SiteIntf] =
        for (i <- indices) yield site(i)


      // TODO: Are `siteByState` and `siteIndexByState` any useful?
      /**
       * Return a given site of this agent as a [[Agent#SiteIntf]].
       *
       * @param s the site state of the site to return.
       * @return the first site with state `s`.
       */
      @inline def siteByState(s: SiteState): SiteIntf =
        site(siteIndexByState(s))

      /**
       * Return the index of the site with the given state.
       *
       * @param s the site state.
       * @return the index of the first site with state `s`.
       */
      @inline def siteIndexByState(s: SiteState): SiteIndex = {
        val i = siteStates.indexWhere { t: SiteState => t == s }
        if (i < 0) throw new NoSuchElementException(
          "agent " + this + " doesn't have a site with state " + s)
        i
      }


      // -- Matchable[SiteGraph#AgentIntf] API --

      /**
       * Compare this agent against `that`.
       *
       * @return `true` if this agent matches `that`.
       */
      def matches(that: SiteGraph#AgentIntf)
          : Boolean = {
        (this.length == that.length) &&
        (this.state matches that.state) &&
        (this.siteStates zip that.siteStates forall {
          case (s1, s2) => s1 matches s2
        }) &&
        (this.links zip that.links forall { case (l1, l2) => l1 matches l2 })
      }

      /**
       * Returns the join of this agent and `that`.
       *
       * ''WARNING'': Agents returned from this method are
       * orphans, i.e. they don't belong to any site graph and should
       * be handled with care.  Also, their links might be
       * problematic, see comment at
       * [[SiteGraphs#SiteGraph.Link.join]].
       *
       * @return the join of `this` and `that`.
       */
      final def join(that: SiteGraph#AgentIntf)
          : Option[SiteGraph#AgentIntf] =
        if (this.length != that.length) None
        else {
          val siteStateJoins = new Array[SiteState](length)
          val linkJoins = new Array[Link](length)
          val allJoins = (indices forall { i =>
            val sj = this.siteStates(i) join that.siteStates(i)
            sj match {
              case Some(s) => siteStateJoins(i) = s; true
              case None    => false
            }
          }) && (indices forall { i =>
            val lj = this.links(i) join that.links(i)
            lj match {
              case Some(l) => linkJoins(i) = l; true
              case None    => false
            }
          })
          if (allJoins)
            for (st <- this.state join that.state)
            yield DummySiteGraph.AgentImpl(this.agentType, st)(
              this.siteTypes, siteStateJoins, linkJoins)
          else None
        }

      /**
       * Returns the meet of this agent and `that`.
       *
       * ''WARNING'': Agents returned from this method are
       * orphans, i.e. they don't belong to any site graph and should
       * be handled with care.  Also, their links might be
       * problematic, see comment at
       * [[SiteGraphs#SiteGraph.Link.meet]].
       *
       * @return the meet of `this` and `that`.
       */
      final def meet(that: SiteGraph#AgentIntf)
          : Option[SiteGraph#AgentIntf] =
        if (this.length != that.length) None
        else {
          val siteStateMeets = new Array[SiteState](length)
          val linkMeets = new Array[Link](length)
          val allMeets = (indices forall { i =>
            val sj = this.siteStates(i) meet that.siteStates(i)
            sj match {
              case Some(s) => siteStateMeets(i) = s; true
              case None    => false
            }
          }) && (indices forall { i =>
            val lj = this.links(i) meet that.links(i)
            lj match {
              case Some(l) => linkMeets(i) = l; true
              case None    => false
            }
          })
          if (allMeets) for (st <- this.state meet that.state)
          yield DummySiteGraph.AgentImpl(this.agentType, st)(
            this.siteTypes, siteStateMeets, linkMeets)
          else None
        }

      /**
       * Checks if this agent can be used in mixtures.
       *
       * @return `true` if this agent is valid in a mixture.
       */
      def isComplete = {
        this.state.isComplete &&
        (this.siteStates forall (_.isComplete)) &&
        (this.links forall (_.isComplete))
      }

      // -- Equals API --
      override def canEqual(that: Any) = that.isInstanceOf[AgentIntf]


      // -- Any API --

      /**
       * Tests whether the argument (`that`) is a reference to the
       * receiver object (`this`).
       *
       * NOTE: There are multiple reasons for using reference equality
       * instead of structural equality for agents:
       *
       *  1) Due to the recursive nature of agents.  Since this trait
       *     contains a sequence of [[SiteGraphs#SiteGraph.Link]]s
       *     (i.e. `links`), checking structural equivalence requires
       *     checking for structural equality of individual links.
       *     But links may contain references to [[Agent]]s (in
       *     particular links that are instances of [[Linked]]).
       *     Hence a naive implementation of structural equality may
       *     end up in a recursive loop.
       *
       *  2) For efficiency.  We really consider different instances
       *     of this class as different agents.
       *
       *  If you want to test for structural equality, use
       *  [[AgentIntf.isEquivTo]] instead.
       *
       * @return `true` if the argument is a reference to the receiver
       *         agent, `false` otherwise.
       */
      override def equals(that: Any): Boolean =
        this.isInstanceOf[AgentIntf] &&
        (this eq that.asInstanceOf[AgentIntf])

      /**
       * Calculate a hash code value for the agent.
       *
       * NOTE: The reasons for overriding the `hashCode` method in
       * this class are the same as those mentioned in
       * [[AgentIntf.equals]].
       *
       * TODO: Is it OK to rely on
       * [[java.lang.System.identityHashCode]]?  A possible alternative
       * would be, to use a counter in the companion object to provide
       * unique (up to counter wrap-around) hash codes when creating an
       * instance of this class.
       *
       * @return the hash code value for this agent.
       */
      override def hashCode(): Int =
        java.lang.System.identityHashCode(this)

      override def toString() = state + sites.mkString("(", ",", ")")
    }


    /**
     * A class representing actual links links between sites.
     *
     * Instances of this class also hold the
     * [[LanguageContext#LinkState]] in the direction from the source
     * site (i.e. the site that stores the instance) to the target
     * site (i.e. the site that is pointed to by the instance).
     *
     * A few words about why target sites are stored in a "relative"
     * fashion, i.e. as (agent, site index) pairs rather than just a
     * reference to the target site.  There are two main reasons for
     * this:
     *
     *  1) Links between sites in [[Mixtures#Mixture]]s are
     *     predominantly created by applying an action (see
     *     [[Actions#Action]]) to a mixture.  An action is a function
     *     that takes a mixture and a sequence of agents as its input
     *     and updates the mixture.  Since the agents that are to be
     *     updated are parameters of the action, the concrete sites to
     *     be modified are not know when the action is created,
     *     instead the action refers to the sites just by their index
     *     w.r.t. the corresponding (formal) agent parameter.  To
     *     simplify the operations that actions perform on agents and
     *     sites it makes sense to store links in an analogous way.
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
    final case class Linked(agent: Agent, site: SiteIndex,
      linkType: ContactGraph.Link, state: LinkState)
        extends Defined
  }


  /** Companion object of [[SiteGraph]]. */
  object SiteGraph {

    /** A class representing links between sites. */
    sealed abstract class Link extends Matchable[Link] {

      // -- Matchable[Link[Agent]] API --

      /**
       * Compare this link against another `that`.
       *
       * @return `true` if `this` matches `that`.
       */
      @inline final
      def matches(that: Link): Boolean = (this, that) match {
        case (Undefined, _) => true
        case (Stub, Stub) => true
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) =>
          (a1 matches a2) && (s1 matches s2) && (l1 matches l2)
        case (Wildcard(a1, s1, l1), l2: SiteGraph#Linked) =>
          (a1 matches Some(l2.agent.state)) &&
          (s1 matches Some(l2.agent.siteStates(l2.site))) &&
          (l1 matches Some(l2.state))
        case (l1: SiteGraph#Linked, l2: SiteGraph#Linked) =>
          (l1.site == l2.site) && (l1.state matches l2.state)
        case _ => false
      }

      /**
       * Returns the join of this link and `that`.
       *
       * NOTE: There is one case that is a little problematic: if we
       * find both `this` and `that` are instances of `Linked`
       * pointing two the same site in two agents `u1` and `u2`, we
       * don't really know what the join of `u1` and `u2` is, and we
       * can't tell whether it even exists, because attempting to
       * compute `u1 join u2` will throw us straight into a recursive
       * loop (the join of `u1` and `u2` does, after all, depend on
       * the join of the links `this` and `that`).  So we'll just
       * assume that `u1` and `u2` do indeed have some join, return
       * the appropriate instance of `Linked` (albeit pointing to `u1`
       * for now), and leave it to whoever wants to traverse the
       * entire site graph to decide whether they want to back-patch
       * the link.
       *
       * @return the join of `this` and `that`.
       */
      final def join(that: Link) = (this, that) match {
        case (Stub, Stub) => Some(Stub)
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) => {
          val wc =
            for (a <- a1 join a2; s <- s1 join s2; l <- l1 join l2)
            yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (Wildcard(a1, s1, l1), l2: SiteGraph#Linked) => {
          val wc = for {
            a <- a1 join Some(l2.agent.state)
            s <- s1 join Some(l2.agent.siteStates(l2.site))
            l <- l1 join Some(l2.state)
          } yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (l1: SiteGraph#Linked, Wildcard(a2, s2, l2)) => {
          val wc = for {
            a <- Some(l1.agent.state)               join a2
            s <- Some(l1.agent.siteStates(l1.site)) join s2
            l <- Some(l1.state)                     join l2
          } yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (l1: SiteGraph#Linked, l2: SiteGraph#Linked) if
          l1.site == l2.site => {
            val u1 = l1.agent; val i1 = l1.site; val t1 = l1.linkType
            val l = l1.state join l2.state
            l map (DummySiteGraph.Linked(
              DummySiteGraph.AgentImpl(u1), i1, t1, _)) orElse {
              val u2 = l2.agent; val i2 = l2.site
              val a = u1.state          join u2.state
              val s = u1.siteStates(i1) join u2.siteStates(i2)
              Some(Wildcard(a, s, l))
            }
          }
        case _ => Some(Undefined)
      }

      /**
       * Returns the meet of this link and `that`.
       *
       * NOTE: This method has the same problem as
       * [[SiteGraphs#SiteGraph.Link.join]], except it may return
       * instances of `Linked` even when only one of the links is an
       * instance of `Linked` and the other one is a compatible
       * Wildcard.
       *
       * @return the meet of `this` and `that`.
       */
      final def meet(that: Link) = (this, that) match {
        case (_, Undefined) => Some(this)
        case (Undefined, _) => Some(that)
        case (Stub, Stub)   => Some(Stub)
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) =>
          for (a <- a1 meet a2; s <- s1 meet s2; l <- l1 meet l2)
          yield Wildcard(a.om, s.om, l.om)
        case (Wildcard(a1, s1, l1), l2: SiteGraph#Linked) =>
          for {
            a <- a1 meet Some(l2.agent.state)
            s <- s1 meet Some(l2.agent.siteStates(l2.site))
            l <- l1 meet Some(l2.state)
          } yield DummySiteGraph.Linked(
            DummySiteGraph.AgentImpl(l2.agent), l2.site,
            l2.linkType, l.om.get)
        case (l1: SiteGraph#Linked, Wildcard(a2, s2, l2)) =>
          for {
            a <- Some(l1.agent.state)               meet a2
            s <- Some(l1.agent.siteStates(l1.site)) meet s2
            l <- Some(l1.state)                     meet l2
          } yield DummySiteGraph.Linked(
            DummySiteGraph.AgentImpl(l1.agent), l1.site,
            l1.linkType, l.om.get)
        case (l1: SiteGraph#Linked, l2: SiteGraph#Linked) if
          l1.site == l2.site => {
            (l1.state meet l2.state) map (DummySiteGraph.Linked(
              DummySiteGraph.AgentImpl(l1.agent), l1.site,
              l1.linkType, _))
          }
        case _ => None
      }

      /**
       * Checks if this link can be used in mixtures.
       *
       * @return `true` if this link is valid in a mixture.
       */
      @inline final def isComplete = this match {
        case Stub                => true
        case _: SiteGraph#Linked => true
        case _                   => false
      }

      // -- Any API --
      // FIXME: missing case Defined... Defined should be sealed
      override def toString = this match {
        case Undefined => "?"
        case Wildcard(a, s, l) =>
          linkDelim + (a getOrElse "_") + "." + (s getOrElse "_") +
          "." + (l getOrElse "_")
        case Stub => ""
        case l: SiteGraph#Linked => linkDelim + l.state
          // linkDelim + l.agent.state + "." + l.agent.siteStates(l.site) + "." + l.state
      }
    }

    /** An object representing undefined sites. */
    case object Undefined extends Link

    /** A class representing defined sites. */
    /*sealed*/ abstract class Defined extends Link

    /** An object representing stubs, i.e. unconnected sites. */
    case object Stub extends Defined

    /**
     * A class representing a wildcard link at a site.
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
      linkState: Option[LinkState]) extends Defined

    /**
     * An object used to construct dummy agents and links returned by
     * the [[Matchable]] API methods in [[SiteGraph#AgentIntf]] and
     * [[SiteGraph.Link]].
     */
    object DummySiteGraph extends SiteGraph {

      /** Dummy agent type. */
      type Agent = AgentImpl

      /**
       * A class representing dummy agents and links returned by the
       * [[Matchable]] API methods in [[SiteGraph#AgentIntf]] and
       * [[SiteGraph.Link]].
       */
      abstract class AgentImpl
          extends AgentIntf {

        val state: AgentState
        val siteTypes: Array[agentType.Site]
        val siteStates: Array[SiteState]
        val links: Array[Link]
      }

      /**
       * The companion object of [[DummySiteGraph.AgentImpl]]
       */
      object AgentImpl {
        def apply(_agentType: ContactGraph.Agent, _state: AgentState)(
          st: Array[_agentType.Site], ss: Array[SiteState],
          ls: Array[Link]): AgentImpl =
          new AgentImpl {
            type T = _agentType.type
            val agentType: T = _agentType
            val state = _state
            val siteStates = ss
            val siteTypes = st
            val links = ls map {
              case l: SiteGraph#Linked =>
                Linked(this, l.site, l.linkType, l.state)
              case l => l
            }
          }

        def apply(u: SiteGraph#AgentIntf): AgentImpl =
          apply(u.agentType, u.state)(
            u.siteTypes, u.siteStates, u.links)
      }
    }
  }
}

