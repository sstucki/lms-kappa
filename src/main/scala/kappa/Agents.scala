package kappa

//import scala.language.implicitConversions
//import scala.language.postfixOps

import scala.collection.mutable

trait Agents {
  this: LanguageContext =>

  /**
   * A class representing agents in site graphs.
   */
  trait Agent extends Matchable[Agent] with Equals {

    /** Subtype of [[Agent]] that sites of this agent may link to. */
    type LinkTarget <: Agent

    /** Type of sites of this agent. */
    type Site = Agent.Site[LinkTarget]

    /** The state of the agent. */
    def state: AgentState

    /** The sites of the agent. */
    def sites: Array[Site]

    /**
     * Returns the neighbor of a given site of this agent if it is
     * connected.
     *
     * @param i site index
     * @return `Some((u, j))`, where `u` and `j` are the neighboring
     *          agent and site index of this agent at site index `i`,
     *          if the site `i` is connected, and `None` otherwise.
     */
    @inline def neighbour(i: SiteIndex): Option[(LinkTarget, SiteIndex)] =
      sites(i).neighbour


    // -- Matchable[Agent] API --

    /**
     * Compare this agent against `that`.
     *
     * @return `true` if this agent matches `that`.
     */
    def matches(that: Agent): Boolean = {
      (this.sites.size == that.sites.size) &&
      (this.state matches that.state) &&
      (this.sites zip that.sites forall { case (s1, s2) => s1 matches s2 })
    }

    /**
     * Returns the join of this agent and `that`.
     *
     * ''WARNING'': Agents returned from this method are
     * orphans, i.e. they don't belong to any site graph and should be
     * handled with care.  Also, their links might be problematic, see
     * comment at [[Link.join]].
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

          val intfs: Option[Vector[Agent.Site[Agent]]] = Some(Vector())
          sitesJoins.foldLeft(intfs) {
            (intfs, siteJoins) =>
            for (intf <- intfs; j <- siteJoins) yield intf :+ j
          }
        }

        for {
          st <- this.state join that.state
          intf <- intfs
        } yield new Agent {
          type LinkTarget = Agent
          val state = st
          val sites = intf.toArray
        }
      }

    /**
     * Returns the meet of this agent and `that`.
     *
     * ''WARNING'': Agents returned from this method are
     * orphans, i.e. they don't belong to any site graph and should be
     * handled with care.  Also, their links might be problematic, see
     * comment at [[Link.meet]].
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

          val intfs: Option[Vector[Agent.Site[Agent]]] = Some(Vector())
          sitesMeets.foldLeft(intfs) {
            (intfs, siteMeets) =>
            for (intf <- intfs; m <- siteMeets) yield intf :+ m
          }
        }

        for {
          st <- this.state meet that.state
          intf <- intfs
        } yield new Agent {
          type LinkTarget = Agent
          val state = st
          val sites = intf.toArray
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
    override def canEqual(that: Any) = that.isInstanceOf[Agent]


    // -- Any API --
    /**
     * Tests whether the argument (`that`) is a reference to the
     * receiver object (`this`).
     *
     * NOTE: There are multiple reasons for using reference equality
     * instead of structural equality for agents:
     *
     *  1) Due to the recursive nature of agents.  Since this trait
     *     contains a sequence of [[Site]]s (i.e. `sites`), checking
     *     structural equivalence requires checking for structural
     *     equality of individual sites.  But sites contain links,
     *     which in turn may contain references to [[Agent]]s (in
     *     particular links that are instances of [[Linked]]).  Hence
     *     a naive implementation of structural equality may end up in
     *     a recursive loop.
     *
     *  2) For efficiency.  We really consider different instances
     *     of this class as different agents.
     *
     *  If you want to test for structural equality, use
     *  [[Agent.isEquivTo]] instead.
     *
     * @return `true` if the argument is a reference to the receiver
     *         agent, `false` otherwise.
     */
    override def equals(that: Any): Boolean =
      that.isInstanceOf[Agent] && (this eq that.asInstanceOf[Agent])

    /**
     * Calculate a hash code value for the agent.
     *
     * NOTE: The reasons for overriding the `hashCode` method in this
     * class are the same as those mentioned in [[Agent.equals]].
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


  /** Companion object of the [[Agents#Agent]] trait. */
  object Agent {

    /**
     * A class representing links between [[Site]]s.
     *
     * @tparam T subtype of [[Agent]] that this class may link to.
     */
    sealed abstract class Link[+T <: Agent] extends Matchable[Link[Agent]] {

      // -- Matchable[Link[Agent]] API --

      /**
       * Compare this link against another `that`.
       *
       * @return `true` if `this` matches `that`.
       */
      @inline final
      def matches(that: Link[Agent]): Boolean = (this, that) match {
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
       * entire site graph to decide whether they want to back-patch
       * the link.
       *
       * @return the join of `this` and `that`.
       */
      final def join(that: Link[Agent]): Option[Link[Agent]] = (this, that) match {
        case (Stub, Stub) => Some(Stub)
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) => {
          val wc =
            for (a <- a1 join a2; s <- s1 join s2; l <- l1 join l2)
            yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (Wildcard(a1, s1, l1), Linked(u2, s2, l2)) => {
          val wc = for {
            a <- a1 join Some(u2.state)
            s <- s1 join Some(u2.sites(s2).state)
            l <- l1 join Some(l2)
          } yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (Linked(u1, s1, l1), Wildcard(a2, s2, l2)) => {
          val wc = for {
            a <- Some(u1.state)           join a2
            s <- Some(u1.sites(s1).state) join s2
            l <- Some(l1)                 join l2
          } yield new Wildcard(a.om, s.om, l.om)
          wc orElse Some(Undefined)
        }
        case (Linked(u1, s1, l1), Linked(u2, s2, l2)) if (s1 == s2) => {
          val l = l1 join l2
          l map (Linked(u1, s1, _)) orElse {
            val a = u1.state           meet u2.state
            val s = u1.sites(s1).state meet u2.sites(s2).state
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
      final def meet(that: Link[Agent]): Option[Link[Agent]] = (this, that) match {
        case (_, Undefined) => Some(this)
        case (Undefined, _) => Some(that)
        case (Stub, Stub)   => Some(Stub)
        case (Wildcard(a1, s1, l1), Wildcard(a2, s2, l2)) =>
          for (a <- a1 meet a2; s <- s1 meet s2; l <- l1 meet l2)
          yield new Wildcard(a.om, s.om, l.om)
        case (Wildcard(a1, s1, l1), Linked(u2, s2, l2)) =>
          // FIXME: See note above.
          for {
            a <- a1 meet Some(u2.state)
            s <- s1 meet Some(u2.sites(s2).state)
            l <- l1 meet Some(l2)
          } yield new Linked(u2, s2, l.om.get)
        case (Linked(u1, s1, l1), Wildcard(a2, s2, l2)) =>
          // FIXME: See note above.
          for {
            a <- Some(u1.state)           meet a2
            s <- Some(u1.sites(s1).state) meet s2
            l <- Some(l1)                 meet l2
          } yield new Linked(u1, s1, l.om.get)
        case (Linked(u1, s1, l1), Linked(u2, s2, l2)) => {
          // FIXME: See note above.
          if (s1 == s2) (l1 meet l2) map (Linked[u1.type](u1, s1, _))
          else None
        }
        case _ => None
      }

      /**
       * Checks if this link can be used in mixtures.
       *
       * @return `true` if this link is valid in a mixture.
       */
      @inline final def isComplete = this match {
        case Stub => true
        case Linked(_, _, _) => true
        case _ => false
      }

      // -- Any API --
      // FIXME!
      override def toString = this match {
        case Undefined => "?"
        case Wildcard(a, s, l) =>
          "!" + (a getOrElse "_") + "." + (s getOrElse "_") +
          "." + (l getOrElse "_")
        case Stub => ""
        case Linked(a, s, l) =>
          "!" + a.state + "." + a.sites(s).state + "." + l
      }
    }

    /** An object representing an undefined link at a [[Site]]. */
    case object Undefined extends Link[Nothing]

    /** An object representing stubs, i.e. unconnected [[Site]]s. */
    case object Stub extends Link[Nothing]

    /**
     * A class representing actual links links between [[Site]]s.
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
     * @tparam T subtype of [[Agent]] that this class may link to.
     * @param agent the target agent of this link.
     * @param site the index of the target site in the target agent of
     *        this link.
     * @param state the state of the link from source to target.
     */
    final case class Linked[T <: Agent](
      agent: T, site: SiteIndex, state: LinkState) extends Link[T]

    /**
     * A class representing a wildcard link at a [[Site]].
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
      linkState: Option[LinkState]) extends Link[Nothing]

    /**
     * A class representing sites in [[Agent]]s.
     *
     * @tparam T subtype of [[Agent]] that this site may link to.
     * @param state the state of this site
     * @param link whether and how this site is linked to another site.
     */
    final case class Site[+T <: Agent](
      val state: SiteState, val link: Link[T] = Undefined)
        extends Matchable[Site[Agent]] {


      /**
       * Returns the neighbor of a site if it is connected.
       *
       * @return `Some((u, j))`, where `u` and `j` are the neighboring
       *          agent and site index of this site if it is
       *          connected, and `None` otherwise.
       */
      @inline def neighbour: Option[(T, SiteIndex)] =
        link match {
          case Linked(u, j, _) => Some(u, j)
          case _ => None
        }


      // -- Matchable[Site[Agent]] API --

      /**
       * Compare this site against another `that`.
       *
       * @return `true` if `this` matches `that`.
       */
      def matches(that: Site[Agent]): Boolean =
        (this.state matches that.state) && (this.link matches that.link)

      /**
       * Returns the join of this site and `that`.
       *
       * @return the join of `this` and `that`.
       */
      final def join(that: Site[Agent]): Option[Site[Agent]] = for {
        s <- this.state join that.state
        l <- this.link join that.link
      } yield new Site(s, l)

      /**
       * Returns the meet of this site and `that`.
       *
       * @return the meet of `this` and `that`.
       */
      final def meet(that: Site[Agent]): Option[Site[Agent]] = for {
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
  }
}

