package kappa

import scala.language.implicitConversions
import scala.language.postfixOps

import scala.collection.mutable

// RHZ: Why _abstract_ syntax?

// TODO: To be able to customise more on the language-specific side
// we need to make everything a trait. In particular, I'm thinking
// about AbstractPattern (which should mix in with Connectors, where
// Connector is defined which should also be a trait?), about Action
// (to let !@ and :@ be language-specific), and Pattern (to let
// randomEmbedding be language-specific because we will need that
// in Spatial Kappa).

/**
 * Generic abstract syntax tree structures for Kappa-like languages.
 *
 * NOTE: The classes in this trait can be used as "pattern
 * combinators", but they are very different form the generic pattern
 * builder in the [[Patterns]] trait.  In fact they are more like the
 * nodes of a pattern AST, enhanced with combinator-like operators.
 */
trait AbstractSyntax {
  syntax: LanguageContext
      with ContactGraphs
      with SiteGraphs
      with Patterns
      with Mixtures
      with Actions
      with Rules =>

  // -- Nodes of abstract syntax trees and their builders. --

  type EndpointName = String


  // -- Partial elements of syntax --

  /**
   * An abstract class representing partial rules.
   *
   * Every abstract syntax node that that may appear in a
   * comma-separated list that can be combined into a rule should
   * extend this class.
   */
  sealed abstract class PartialAbstractRule

  /**
   * An abstract class representing partially built abstract patterns.
   *
   * Every abstract syntax node that can be converted into a pattern
   * (i.e. that can be used in pattern position) should extend this
   * class.
   */
  abstract class PartialAbstractPattern extends PartialAbstractRule {

    /** Convert this partial abstract pattern into an abstract pattern. */
    def toAbstractPattern: AbstractPattern

    /** Append an abstract agent to this partial abstract pattern.  */
    @inline def :+(that: AbstractAgent): AbstractPattern =
      this.toAbstractPattern :+ that

    /** Append an abstract action to this partial abstract pattern.  */
    @inline def :+(that: AbstractAction): AbstractAction = this ++ that

    /** Append an abstract bidirectional action to this partial abstract pattern.  */
    @inline def :+(that: AbstractBiAction): AbstractBiAction = this ++ that

    /** Append an abstract pattern to this partial abstract pattern.  */
    @inline def ++(that: AbstractPattern): AbstractPattern =
      this.toAbstractPattern ++ that

    /** Append an abstract action to this partial abstract pattern.  */
    @inline def ++(that: AbstractAction): AbstractAction =
      AbstractAction(this ++ that.lhs, that.rhs)

    /** Append an abstract bidirectional action to this partial abstract pattern.  */
    @inline def ++(that: AbstractBiAction): AbstractBiAction =
      AbstractBiAction(this ++ that.lhs, that.rhs)

    /** Append an abstract rule tail to this partial abstract pattern.  */
    @inline def ++(that: AbstractRuleTail): AbstractRuleTail =
      that.prependAbstractPattern(this.toAbstractPattern)

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def +:(that: AbstractAgent): AbstractPattern =
      that +: this.toAbstractPattern

    /** Prepend an abstract action to this partial abstract pattern.  */
    @inline def +:(that: AbstractAction): AbstractAction =
      that ++ this.toAbstractPattern

    /** Prepend an abstract bidirectional action to this partial abstract pattern.  */
    @inline def +:(that: AbstractBiAction): AbstractBiAction =
      that ++ this.toAbstractPattern

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def ::(that: AbstractAgent): AbstractPattern =
      that +: this

    /** Prepend an abstract action to this partial abstract pattern.  */
    @inline def ::(that: AbstractAction): AbstractAction =
      that +: this

    /** Prepend an abstract bidirectional action to this partial abstract pattern.  */
    @inline def ::(that: AbstractBiAction): AbstractBiAction =
      that +: this

    /**
     * Build an abstract action from this partial abstract pattern and
     * an abstract agent.
     */
    @inline def ->(that: AbstractAgent): AbstractAction =
      AbstractAction(this.toAbstractPattern, that.toAbstractPattern)

    /** Build an action from this partial abstract pattern and a pattern. */
    @inline def ->(that: AbstractPattern)(implicit ab: ActionBuilder)
        : ab.RuleBuilder = ab(this.toPattern, that.toPattern)

    @inline def ->(that: AbstractRuleTail)(implicit ab: ActionBuilder)
        : AbstractRule = {
      val lhs = this.toAbstractPattern
      AbstractRule(AbstractAction(lhs, that.rhs), that.toRate(lhs))
    }

    /**
     * Build an action from this partial abstract pattern and the
     * empty pattern.
     */
    @inline def ->()(implicit ab: ActionBuilder): ab.RuleBuilder =
      ab(this.toPattern, AbstractPattern().toPattern)

    /**
     * Build an abstract bidirectional action from this partial
     * abstract pattern and an abstract agent.
     */
    @inline def <->(that: AbstractAgent): AbstractBiAction =
      AbstractBiAction(this.toAbstractPattern, that.toAbstractPattern)

    /** Build a bidirectional action from this partial abstract pattern and a pattern. */
    @inline def <->(that: AbstractPattern)(implicit bab: BiActionBuilder)
        : bab.BiRuleBuilder = bab(this.toPattern, that.toPattern)

    @inline def <->(that: AbstractBiRuleTail)(
      implicit bab: BiActionBuilder): AbstractBiRule = {
      val lhs = this.toAbstractPattern
      AbstractBiRule(AbstractBiAction(lhs, that.rhs),
        that.toFwdRate(lhs), that.toBwdRate(that.rhs))
    }

    /**
     * Build a bidirectional action from this partial abstract pattern
     * and the empty pattern.
     */
    @inline def <->()(implicit bab: BiActionBuilder): bab.BiRuleBuilder =
      bab(this.toPattern, AbstractPattern().toPattern)

    // FIXME: :@ and !@ methods should be language-specific.
    // (Abstract)(Bi)RuleBuilder should be a trait that we mix-in
    // AbstractPatterns and Actions.
    /**
     * Build the tail of a rule that follows mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant
     */
    @inline def :@(rate: => Double): AbstractMassActionRuleTail =
      AbstractMassActionRuleTail(this.toAbstractPattern, () => rate)

    /**
     * Build the tail of a bidirectional rule that follows
     * mass-action kinetics.
     *
     * @param fwdRate stochastic kinetic rate constant for the
     *        forward rule.
     * @param bwdRate stochastic kinetic rate constant for the
     *        backward rule.
     */
    @inline def :@(fwdRate: => Double, bwdRate: => Double)
        : AbstractMassActionBiRuleTail =
      AbstractMassActionBiRuleTail(this.toAbstractPattern,
        () => fwdRate, () => bwdRate)

    /**
     * Build the tail of a rule that follows an arbitrary rate law.
     *
     * @param law kinetic law expression
     */
    @inline def !@(law: => Double): AbstractRateLawRuleTail =
      AbstractRateLawRuleTail(this.toAbstractPattern, () => law)

    /**
     * Build the tail of a bidirectional rule that follows
     * an arbitrary rate law.
     *
     * @param fwdLaw stochastic kinetic law for the forward rule.
     * @param bwdRate stochastic kinetic law for the backward rule.
     */
    @inline def !@(fwdLaw: => Double, bwdLaw: => Double)
        : AbstractRateLawBiRuleTail =
      AbstractRateLawBiRuleTail(this.toAbstractPattern,
        () => fwdLaw, () => bwdLaw)

    // TODO: Add tokens?

    /**
     * Build a mixture with `x` copies of this abstract pattern.
     *
     * @param x the number of copies of this abstract pattern in the
     *        resulting mixture.
     */
    @inline def *(x: Int): Mixture = this.toMixture * x

    /** Convert this partial abstract pattern into a pattern. */
    @inline def toPattern: Pattern = this.toAbstractPattern.toPattern

    /** Convert this partial abstract pattern into a mixture. */
    @inline def toMixture: Mixture = this.toAbstractPattern.toMixture
  }

  // TODO: Are partial sites and agents really necessary?
  // Can not we put these types, values and methods in AbstractAgent
  // and AbstractSite directly?
  // /** An abstract class representing partially built abstract sites.
  //   *
  //   * Every abstract syntax node that can be converted into a site
  //   * (i.e. that can be used in site position) should extend this
  //   * class.
  //   */
  // abstract class PartialAbstractSite {
  //   type State <: AbstractSiteState
  //   val siteType: ContactGraph.Site
  //   /** Convert this partial abstract site into an abstract site. */
  //   def toAbstractSite: AbstractSite[State]
  // }

  // /**
  //  * An abstract class representing partially built abstract agents.
  //  *
  //  * Every abstract syntax node that can be converted into an agent
  //  * (i.e. that can be used in agent position) should extend this
  //  * class.
  //  */
  // abstract class PartialAbstractAgent extends PartialAbstractPattern {
  //   // type State <: AbstractAgentState
  //   val agentType: ContactGraph.Agent
  //   /** Convert this partial abstract agent into an abstract agent. */
  //   def toAbstractAgent: AbstractAgent//[State]
  //   /** Convert this partial abstract agent into an abstract pattern. */
  //   @inline def toAbstractPattern: AbstractPattern =
  //     this.toAbstractAgent.toAbstractPattern
  // }


  // -- Complete elements of syntax --

  // - Links -

  /** A class representing abstract link states. */
  abstract class AbstractLinkState {

    /** Creates a link state from this abstract link state. */
    def toLinkState(set: LinkStateSet, linkId: Option[LinkId])
        : LinkState
  }


  /** A class representing abstract links. */
  sealed abstract class AbstractLink

  /** A class representing undefined links at abstract sites. */
  final case object AbstractUndefined extends AbstractLink

  /** A class representing free abstract sites. */
  final case object AbstractStub extends AbstractLink

  /** A class representing wildcard links at abstract sites. */
  final case class AbstractWildcard(
    agentState: Option[AbstractAgentState],
    siteState: Option[AbstractSiteState],
    linkState: Option[AbstractLinkState]) extends AbstractLink

  /** A class representing abstract links between abstract sites. */
  abstract class AbstractLinked extends AbstractLink {

    /** The link ID of this abstract link. */
    def id: LinkId

    /** The state of this abstract link. */
    def state: AbstractLinkState
  }

  final case class AbstractEndpoint(name: EndpointName)
      extends AbstractLink


  // - Sites -

  /** A class representing abstract site states. */
  abstract class AbstractSiteState {

    def siteType(agentType: ContactGraph.Agent): ContactGraph.Site

    /** Build an undefined abstract site from this partial abstract
      * site.
      */
    @inline def ?() = AbstractSite(this, AbstractUndefined)

    /** Build a free abstract site from this partial abstract site. */
    @inline def !-() = AbstractSite(this, AbstractStub)

    /** Build an abstract site with a wildcard link from this partial
      * abstract site.
      *
      * @param wc the wildcard connected to this partial abstract site.
      */
    @inline def !(
      agentState: Option[AbstractAgentState],
      siteState: Option[AbstractSiteState],
      linkState: Option[AbstractLinkState]): AbstractSite =
      this ! AbstractWildcard(agentState, siteState, linkState)

    /** Build an abstract site with a maximally general wildcard link
      * from this partial abstract site.
      */
    @inline def !*() = this ! (None, None, None)

    /** Build a linked abstract site from this partial abstract site.
      *
      * @param link the link connecting this partial abstract site.
      */
    @inline def !(link: AbstractLink) =
      AbstractSite(this, link)

    @inline def /(endpointName: EndpointName) =
      AbstractSite(this, AbstractEndpoint(endpointName))

    @inline def toAbstractSite = this.!-

    /** Creates a site state from this abstract site state. */
    def toSiteState(agentType: ContactGraph.Agent): SiteState
  }

  // - Agents -

  /** A class representing abstract agent states. */
  abstract class AbstractAgentState {

    def agentType: ContactGraph.Agent

    /** Build an abstract agent from this partial abstract agent.
      *
      * @param sites a sequence of partial abstract sites representing
      *        the interface of the abstract agent to build.
      */
    def apply(sites: AbstractSiteState*): AbstractAgent = {
      // Convert partial sites into total sites and
      // put them into an AbstractAgent with `this` state
      new AbstractAgent(this, sites map (_.toAbstractSite))
    }

    @inline def toAbstractAgent = apply()

    /** Creates an agent state from this abstract agent state. */
    def toAgentState: AgentState
  }

  /** A class representing abstract sites. */
  final case class AbstractSite(
    val state: AbstractSiteState,
    val link: AbstractLink)

  /** A class representing abstract agents. */
  final case class AbstractAgent(
    val state: AbstractAgentState,
    val sites: Seq[AbstractSite]) {

    def agentType: ContactGraph.Agent = state.agentType

    def undefineSites(siteIndices: Set[SiteIndex]): AbstractAgent =
      if (siteIndices.isEmpty) this
      else new AbstractAgent(state,
        for ((s, i) <- sites.zipWithIndex) yield
          if (siteIndices contains i)
            AbstractSite(s.state, AbstractUndefined)
          else s)

    def closeWith(n: Int, f: EndpointName => AbstractLink)
        : (AbstractAgent, Int) = {

      val (_sites: List[AbstractSite], m) = sites.foldLeft(
        (List[AbstractSite](), n))({
          case ((sites, n), AbstractSite(state, AbstractEndpoint(ep)))
              if n > 0 => (AbstractSite(state, f(ep)) :: sites, n-1)
          case ((sites, n), s) => (s :: sites, n)
        })
      (new AbstractAgent(state, _sites.reverse), m)
    }

    def addToPattern(pb: Pattern.Builder) {
      val at = state.agentType
      val v = pb += (at, state.toAgentState)
      for (s <- sites)
        v += (s.state.siteType(at), s.state.toSiteState(at))
    }

    @inline def toAbstractAgent = this

    @inline def toAbstractPattern: AbstractPattern =
      AbstractPattern(Vector(this))
  }


  // - Patterns -

  // TODO: Make this class abstract... Why? I guess I wrote this
  // because I want to make some things extension-dependent (eg
  // Connectors in normal Kappa)
  /** A class representing abstract patterns. */
  final case class AbstractPattern(
    agents: Vector[AbstractAgent] = Vector(),
    links: Seq[(AgentIndex, SiteIndex, AbstractLinkState,
                AgentIndex, SiteIndex, AbstractLinkState)] = List())
      extends PartialAbstractPattern {
    pattern =>

    @inline override def :+(that: AbstractAgent): AbstractPattern =
      AbstractPattern(agents :+ that, links)

    @inline override def +:(that: AbstractAgent): AbstractPattern =
      AbstractPattern(that +: agents, links)

    @inline override def ++(that: AbstractPattern): AbstractPattern = {
      val offset = this.agents.length
      AbstractPattern(this.agents ++ that.agents,
        this.links ++ (
          // Offset `that.links`'s agent ids
          for ((i1, j1, ls1, i2, j2, ls2) <- that.links) yield {
            (i1 + offset, j1, ls1, i2 + offset, j2, ls2)
          }))
    }

    def isEmpty: Boolean = agents.isEmpty

    def nonEmpty: Boolean = agents.nonEmpty

    def reverse: AbstractPattern =
      new AbstractPattern(agents.reverse,
        for ((i1, j1, ls1, i2, j2, ls2) <- links)
        yield (agents.length - i1 - 1, j1, ls1,
               agents.length - i2 - 1, j2, ls2))

    // This is necessary because Scala can't apply to implicit
    // conversions in a row, so when we do "A(x)".inMix, it won't
    // work.
    @inline def inMix: Int = this.toPattern.inMix

    // *** Connectors ***

    // RHZ: How can I add things on the language-specific side here?
    // For instance, I'd like to add a + method in Kappa that doesn't
    // require the AbstractLinkState.
    // I'd probably need to make AbstractPattern an abstract class

    final class Connector(
      eps: List[(EndpointName, AbstractLinkState,
                 AbstractLinkState, EndpointName)]) {

      def +(ep1: EndpointName, ls1: AbstractLinkState,
            ls2: AbstractLinkState, ep2: EndpointName) =
        new Connector((ep1, ls1, ls2, ep2) :: eps)

      def +(xs: Seq[(EndpointName, AbstractLinkState,
                     AbstractLinkState, EndpointName)]) =
        new Connector(xs.toList ++ eps)

      def >-(that: AbstractPattern): AbstractPattern = {

        def openEndpoints(ap: AbstractPattern)
            : Map[EndpointName, Seq[(AgentIndex, SiteIndex)]] = {
          for ((u, i) <- ap.agents.zipWithIndex;
               (s, j) <- u.sites.zipWithIndex)
          yield s.link match {
            case AbstractEndpoint(_) => Some((i, j))
            case _ => None
          }
        }.flatten.groupBy {
          case (i, j) => ap.agents(i).sites(j).link match {
            case AbstractEndpoint(ep) => ep
            case _ => throw new IllegalStateException()
          }
        }

        val eps1: mutable.HashMap[EndpointName, Seq[(AgentIndex, SiteIndex)]] =
          new mutable.HashMap()
        for ((ep1, xs) <- openEndpoints(pattern)) eps1 += ((ep1, xs))

        val eps2: mutable.HashMap[EndpointName, Seq[(AgentIndex, SiteIndex)]] =
          new mutable.HashMap()
        for ((ep2, xs) <- openEndpoints(that)) eps2 += ((ep2, xs))

        val offset = pattern.agents.length

        // TODO: This should be done lazily
        val links = for {
          (ep1, ls1, ls2, ep2) <- eps
          ((i1, j1) +: ep1rest) <- eps1 get ep1
          ((i2, j2) +: ep2rest) <- eps2 get ep2
        } yield {
          eps1 += ((ep1, ep1rest))
          eps2 += ((ep2, ep2rest))
          (i1, j1, ls1, offset + i2, j2, ls2)
        }

        def undefineSites(agents: Seq[AbstractAgent],
          siteMap: Map[AgentIndex, Set[SiteIndex]]): Seq[AbstractAgent] =
          for ((u, i) <- agents.zipWithIndex)
          yield u undefineSites siteMap(i)
            // if (siteMap(i).nonEmpty)
            //   u.state.AbstractAgent(
            //     for ((s: u.state.AbstractSite, j) <- u.sites.zipWithIndex)
            //     yield if (siteMap(i) contains j)
            //             u.state.AbstractSite(s.state, AbstractUndefined)
            //           else s)
            // else u

        val sm1 = links.foldLeft(Map() withDefaultValue
          Set(): Map[AgentIndex, Set[SiteIndex]]) {
            case (sm, (i1, j1, _, _, _, _)) =>
              sm + ((i1, sm(i1) + j1)) }

        val sm2 = links.foldLeft(Map() withDefaultValue
          Set(): Map[AgentIndex, Set[SiteIndex]]) {
            case (sm, (_, _, _, i2, j2, _)) =>
              sm + ((i2 - offset, sm(i2 - offset) + j2)) }

        val newAgents =
          undefineSites(pattern.agents, sm1) ++
          undefineSites(that.agents, sm2)

        val thatLinks =
          for ((i1, j1, ls1, i2, j2, ls2) <- that.links)
          yield (offset + i1, j1, ls1, offset + i2, j2, ls2)

        AbstractPattern(newAgents.toVector,
          pattern.links ++ thatLinks ++ links.toList)
      }
    }

    /** Connect this abstract pattern (say `p1`) to another abstract
      * pattern (say `p2`) using the open endpoint `ep1` in `p1` and
      * `ep2` in `p2`.  The resulting link has states `ls1`, `ls2`.
      */
    def -<(ep1: EndpointName, ls1: AbstractLinkState,
           ls2: AbstractLinkState, ep2: EndpointName) =
      new Connector(List((ep1, ls1, ls2, ep2)))

    def -<(xs: Seq[(EndpointName, AbstractLinkState,
                    AbstractLinkState, EndpointName)]) =
      new Connector(xs.toList)

    /** Iterates over the first `n` open endpoints in this abstract
      * pattern and replace them using `f`.
      */
    def closeWith(n: Int, f: EndpointName => AbstractLink)
        : AbstractPattern = {
      // var i = 0
      val (newAgents,_) = agents.foldLeft(
        (Vector[AbstractAgent](), n))({
          case ((newAgents,n),u) => {
            val (newAgent,m) = u.closeWith(n,f)
            (newAgent +: newAgents,m)
          }
        })
      // val newAgents =
      //   for (u <- agents) yield u.state.AbstractAgent(
      //     for (s <- u.sites) yield s.link match {
      //       case AbstractEndpoint(ep) if i < n => {
      //         i += 1
      //         u.state.AbstractSite(s.state, f(ep))
      //       }
      //       case _ => s
      //     })
      AbstractPattern(newAgents.toVector, links)
    }

    def close(n: Int): AbstractPattern =
      closeWith(n, { _ => AbstractStub })

    /** Iterates over all open endpoints in this abstract pattern and
      * replace them using `f`.
      */
    def closeAllWith(f: EndpointName => AbstractLink)
        : AbstractPattern = closeWith(Int.MaxValue, f)

    /** Replace all open endpoints by stubs (i.e. free sites) in this
      * abstract pattern.
      */
    def closeAll: AbstractPattern =
      closeWith(Int.MaxValue, { _ => AbstractStub })


    @inline override def toAbstractPattern: AbstractPattern = this

    override def toPattern: Pattern = {

      // Create a builder to build this pattern
      val pb = new Pattern.Builder()

      // Create a map to track which sites a given link connects.
      val linkMap =
        new mutable.ArrayBuffer[(pb.Agent#Site, AbstractLinked)]

      val siteMap =
        new mutable.HashMap[(AgentIndex, SiteIndex), pb.Agent#Site]

      for (u <- agents) u.addToPattern(pb)

      // Create agents
      for ((u, i) <- agents.zipWithIndex) {

        // // Create the concrete site states and complete the interface
        // // by filling in undefined sites states.
        // val sites = (for ((s, j) <- u.sites.zipWithIndex)
        //              yield (s.state.toSiteState, (s, j))).toMap

        // val siteStates = u.agentType.completeInterface(sites.keys.toSeq)
        // val intf = siteStates map { siteState =>
        //   (siteState, sites.get(siteState) map (_._1.link) getOrElse
        //     AbstractUndefined) }

        // Create the concrete agent state from the abstract one and
        // add the state to the builder to get an builder agent.
        // val v = pb += (u.state.agentType, u.state.toAgentState)
        val v = pb.agents(i)

        for ((s, j) <- u.sites.zipWithIndex) {

          // val x = v += (s.state.siteType, s.state.toSiteState)
          val x = v.sites(j)

          // if (sites contains siteState)
          //   siteMap += ((i, sites(siteState)._2) -> x)
          siteMap += ((i, j) -> x)

          s.link match {
            case AbstractUndefined => ()

            case AbstractStub =>
              x define SiteGraph.Stub

            case AbstractWildcard(agentState, siteState, linkState) => {
              // TODO: Perhaps I should not care about link types
              // source and target...
              // TODO: Should this be lazy?
              val linkStateSets = for {
                as <- agentState.toSeq
                ss <- siteState.toSeq
                st = ss.siteType(as.agentType)
                ContactGraph.Link(siteType, states) <- st.links
                if siteType == st
              } yield states
              require(linkStateSets.length > 0, "link not found")
              require(linkStateSets.length < 2,
                "too many link state sets")
              x define SiteGraph.Wildcard(
                for (as <- agentState) yield as.toAgentState,
                for (as <- agentState; ss <- siteState)
                yield ss.toSiteState(as.agentType),
                for (ls <- linkState)
                yield ls.toLinkState(linkStateSets.head, None))
            }

            case l: AbstractLinked => linkMap += (x -> l)

            case AbstractEndpoint(ep) =>
              throw new IllegalStateException("abstract pattern " +
                "still has open endpoints (e.g. " + ep + ")")
          }
        }
      }

      def findLinkType(from: pb.Agent#Site, to: pb.Agent#Site) =
        from.siteType.links find {
          case ContactGraph.Link(cgTo, states) => to.siteType == cgTo
        } getOrElse {
          throw new IllegalStateException("couldn't find link in " +
            "contact graph between agents " + from + " and " + to)
        }

      // Connect links
      for (l <- linkMap groupBy { case (_, l) => l.id }) l match {
        case (id, Seq((x1, l1), (x2, l2))) => {
          // val t1 = s1.state.siteStateSet
          // val t2 = s2.state.siteStateSet
          // val ls1 = l1.toLinkState(Some(id), t1, t2)
          // val ls2 = l2.toLinkState(Some(id), t2, t1)
          // Get link types and build concrete link states
          val lt1 = findLinkType(x1, x2)
          val lt2 = findLinkType(x2, x1)
          x1 connect (x2,
            lt1, l1.state.toLinkState(lt1.states, Some(id)),
            lt2, l2.state.toLinkState(lt2.states, Some(id)))
        }
        case (id, Seq(_)) => throw new IllegalStateException(
          "dangling link with label " + id)
        case (id, xs) => throw new IllegalStateException(
          "attempt to create hyperlink with label " + id +
          " to connect sites: " + (xs map (_._1.state)).mkString(", "))
      }

      // Connect open endpoints
      for ((i1, j1, l1, i2, j2, l2) <- links) {
        val x1 = siteMap((i1, j1))
        val x2 = siteMap((i2, j2))
        // // val ss1 = x1.state.siteStateSet
        // // val ss2 = x2.state.siteStateSet
        // val x1 = pb.agents(i1).sites(j1)
        // val x2 = pb.agents(i2).sites(j2)
        // Get link types and build concrete link states
        val lt1 = findLinkType(x1, x2)
        val lt2 = findLinkType(x2, x1)
        x1 connect (x2,
          lt1, l1.toLinkState(lt1.states, None), //, ss1, ss2),
          lt2, l2.toLinkState(lt2.states, None)) //, ss2, ss1))
      }

      // Build the pattern
      pb.build
    }

    // FIXME: There's code duplication with toPattern.
    /*
    override def toMixture: Mixture = {

      val m = new Mixture

      // Create a map to track which sites a given link connects.
      val linkMap = new mutable.HashMap[LinkId, List[(Mixture.Agent,
        SiteIndex, AbstractLinkState)]]() // withDefaultValue Nil

      for (u <- agents) {

        // val agentState = u.state.toAgentState
        // val agentStateSet = agentState.agentStateSet

        // val siteStates = new Array[SiteState](u.sites.length)
        // for ((s, i) <- u.sites.zipWithIndex)
        //   siteStates(i) = s.state.toSiteState

        val v = new Mixture.Agent(u.state.toAgentState,
          (u.sites map (_.toSiteState)).toArray,
          new Array[SiteGraph.Link](u.sites.length))

        for ((s, i) <- u.sites.zipWithIndex)
          v._links(i) = s.link match {
            case AbstractUndefined => SiteGraph.Undefined
            case AbstractStub => SiteGraph.Stub
            case AbstractWildcard(aas, ass, als) => {
              // a{a,s,l}s: abstract {agent,site,link} state
              val as = for (aas <- aas) yield aas.toAgentState
              val ss = for (as <- as; ass <- ass) yield
                ass.toSiteState //(as.agentStateSet)
              val ls = for (ss <- ss; als <- als) yield
                als.toLinkState(None) //, siteStates(i).siteStateSet, ss.siteStateSet)
              SiteGraph.Wildcard(as, ss, ls)
            }
            case l: AbstractLinked => {
              linkMap += ((l.id, (v, i, l.state) :: linkMap(l.id)))
              SiteGraph.Undefined
            }
            case AbstractEndpoint(ep) =>
              throw new IllegalStateException("abstract pattern " +
                "still has open endpoints (e.g. " + ep + ")")
          }

        m += v
      }

      // Connect links
      for (l <- linkMap) l match {
        case (id, List((u1, i1, l1), (u2, i2, l2))) => {
          // val t1 = u1.siteStates(i1).siteStateSet
          // val t2 = u2.siteStates(i2).siteStateSet
          val ls1 = l1.toLinkState(Some(id)) //, t1, t2)
          val ls2 = l2.toLinkState(Some(id)) //, t2, t1)
          m connect (u1, i1, ls1, u2, i2, ls2)
        }
        case (_, List()) => ()
        case (id, List(_)) => throw new IllegalStateException(
          "dangling link with label " + id)
        // FIXME: fix error message
        case (id, xs) => throw new IllegalStateException(
          "attempt to create hyperlink with label " + id +
          " to connect sites: " + (xs map (_._1.state)).mkString(", "))
      }

      // Connect open endpoints
      for ((i1, j1, ls1, i2, j2, ls2) <- links) {
        val u1 = m(i1)
        val u2 = m(i2)
        // val ss1 = u1.siteStates(j1).siteStateSet
        // val ss2 = u2.siteStates(j2).siteStateSet
        m connect (u1, i1, ls1.toLinkState(None), //, ss1, ss2),
                   u2, i2, ls2.toLinkState(None)) //, ss2, ss1))
      }

      m
    }
    */
  }


  /** A class representing abstract actions. */
  final case class AbstractAction(lhs: AbstractPattern,
    rhs: AbstractPattern) extends PartialAbstractRule {

    /** Append an abstract agent to this abstract action.  */
    @inline def :+(that: AbstractAgent): AbstractAction =
      AbstractAction(lhs, rhs :+ that)

    /** Append an abstract pattern to this abstract action.  */
    @inline def ++(that: AbstractPattern): AbstractAction =
      AbstractAction(lhs, rhs ++ that)

    /** Append an abstract rule tail to this abstract action.  */
    @inline def ++(that: AbstractRuleTail)(implicit ab: ActionBuilder)
        : AbstractRule = {
      val action = AbstractAction(lhs, this.rhs ++ that.rhs)
      AbstractRule(action, that.toRate(action.lhs))
    }

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def +:(that: AbstractAgent): AbstractAction =
      AbstractAction(that +: lhs, rhs)

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def ::(that: AbstractAgent): AbstractAction =
      that +: this

    // FIXME: :@ and !@ methods should be language-specific
    /**
     * Build a rule that follows mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant.
     */
    def :@(rate: => Double)(implicit ab: ActionBuilder) =
      AbstractRule(this, () => lhs.inMix * rate)

    /**
     * Build a rule that follow an arbitrary rate law.
     *
     * @param law kinetic law expression
     */
    def !@(law: => Double)(implicit ab: ActionBuilder) =
      AbstractRule(this, () => law)

    /** Convert this abstract action into an action. */
    @inline def toAction()(implicit ab: ActionBuilder): Action =
      ab(lhs.toPattern, rhs.toPattern).getAction
  }

  /** A class representing abstract bidirectional actions. */
  final case class AbstractBiAction(lhs: AbstractPattern,
    rhs: AbstractPattern) extends PartialAbstractRule {

    /** Append an abstract agent to this abstract bidirectional action. */
    @inline def :+(that: AbstractAgent): AbstractBiAction =
      AbstractBiAction(lhs, rhs :+ that)

    /** Append an abstract pattern to this abstract bidirectional action. */
    @inline def ++(that: AbstractPattern): AbstractBiAction =
      AbstractBiAction(lhs, rhs ++ that)

    /** Append an abstract rule tail to this abstract bidirectional action. */
    @inline def ++(that: AbstractBiRuleTail)(
      implicit bab: BiActionBuilder): AbstractBiRule = {
      val rhs = this.rhs ++ that.rhs
      AbstractBiRule(AbstractBiAction(lhs, rhs), that.toFwdRate(lhs),
        that.toBwdRate(rhs))
    }

    /** Prepend an abstract agent to this abstract bidirectional action. */
    @inline def +:(that: AbstractAgent): AbstractBiAction =
      AbstractBiAction(that +: lhs, rhs)

    /** Prepend an abstract agent to this abstract bidirectional action. */
    @inline def ::(that: AbstractAgent): AbstractBiAction =
      that +: this

    // FIXME: :@ and !@ methods should be language-specific
    /**
     * Build a bidirectional rule that follows mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant
     */
    def :@(fwdRate: => Double, bwdRate: => Double)(
      implicit bab: BiActionBuilder): AbstractBiRule =
      AbstractBiRule(this, () => lhs.inMix * fwdRate,
        () => rhs.inMix * bwdRate)

    /**
     * Build a bidirectional rule that follow an arbitrary rate law.
     *
     * @param law kinetic law expression
     */
    def !@(fwdLaw: => Double, bwdLaw: => Double)(
      implicit bab: BiActionBuilder): AbstractBiRule =
      AbstractBiRule(this, () => fwdLaw, () => bwdLaw)

    /** Convert this abstract bidirectional action into a
      * bidirectional action.
      */
    @inline def toBiAction()(implicit bab: BiActionBuilder): BiAction =
      bab(lhs.toPattern, rhs.toPattern).getBiAction
  }

  /** A class representing tails of rules. */
  sealed abstract class AbstractRuleTail extends PartialAbstractRule {

    /** The right-hand side of the rule of which this is the tail. */
    def rhs: AbstractPattern

    /** Prepend an agent to the right-hand side of this tail. */
    def +:(that: AbstractAgent): AbstractRuleTail

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractRuleTail

    /** Build a final rate law expression given a left-hand side. */
    def toRate(lhs: Pattern): () => Double

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def ::(that: AbstractAgent): AbstractRuleTail =
      that +: this
  }

  /** A class representing tails of rules that follow mass-action kinetics. */
  final case class AbstractMassActionRuleTail(
    rhs: AbstractPattern, rate: () => Double) extends AbstractRuleTail {

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def +:(that: AbstractAgent): AbstractRuleTail =
      AbstractMassActionRuleTail(that +: rhs, rate)

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractRuleTail =
      AbstractMassActionRuleTail(that ++ rhs, rate)

    /** Build a final rate law expression given a left-hand side. */
    @inline def toRate(lhs: Pattern): () => Double =
      () => lhs.inMix * rate()
  }

  /** A class representing tails of rules that follow arbitrary rate laws. */
  final case class AbstractRateLawRuleTail(rhs: AbstractPattern,
    law: () => Double) extends AbstractRuleTail {

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def +:(that: AbstractAgent): AbstractRuleTail =
      AbstractRateLawRuleTail(that +: rhs, law)

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractRuleTail =
      AbstractRateLawRuleTail(that ++ rhs, law)

    /** Build a final rate law expression given a left-hand side. */
    @inline def toRate(lhs: Pattern): () => Double = law
  }

  /** A class representing tails of bidirectional rules. */
  sealed abstract class AbstractBiRuleTail extends PartialAbstractRule {

    /** The right-hand side of the bidirectional rule of which this is the tail. */
    def rhs: AbstractPattern

    /** Prepend an agent to the right-hand side of this tail. */
    def +:(that: AbstractAgent): AbstractBiRuleTail

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractBiRuleTail

    /** Build a final forward rate law expression given a left-hand side. */
    def toFwdRate(lhs: Pattern): () => Double

    /** Build a final backward rate law expression given a left-hand side. */
    def toBwdRate(lhs: Pattern): () => Double

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def ::(that: AbstractAgent): AbstractBiRuleTail =
      that +: this
  }

  /** A class representing tails of bidirectional rules that follow mass-action kinetics. */
  final case class AbstractMassActionBiRuleTail(
    rhs: AbstractPattern, fwdRate: () => Double, bwdRate: () => Double)
      extends AbstractBiRuleTail {

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def +:(that: AbstractAgent): AbstractBiRuleTail =
      AbstractMassActionBiRuleTail(that +: rhs, fwdRate, bwdRate)

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractBiRuleTail =
      AbstractMassActionBiRuleTail(that ++ rhs, fwdRate, bwdRate)

    /** Build a final forward rate law expression given a left-hand side. */
    @inline def toFwdRate(lhs: Pattern): () => Double =
      () => lhs.inMix * fwdRate()

    /** Build a final backward rate law expression given a left-hand side. */
    @inline def toBwdRate(lhs: Pattern): () => Double =
      () => lhs.inMix * bwdRate()
  }

  /** A class representing tails of bidirectional rules that follow arbitrary rate laws. */
  final case class AbstractRateLawBiRuleTail(
    rhs: AbstractPattern, fwdLaw: () => Double, bwdLaw: () => Double)
      extends AbstractBiRuleTail {

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def +:(that: AbstractAgent): AbstractBiRuleTail =
      AbstractRateLawBiRuleTail(that +: rhs, fwdLaw, bwdLaw)

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractBiRuleTail =
      AbstractRateLawBiRuleTail(that ++ rhs, fwdLaw, bwdLaw)

    /** Build a final forward rate law expression given a left-hand side. */
    @inline def toFwdRate(lhs: Pattern): () => Double = fwdLaw

    /** Build a final backward rate law expression given a left-hand side. */
    @inline def toBwdRate(lhs: Pattern): () => Double = bwdLaw
  }

  sealed abstract class AbstractRuleBox extends PartialAbstractRule {
    def toRule: RuleBox
  }

  /** A class representing abstract Rules. */
  final case class AbstractRule(action: AbstractAction,
    law: () => Double)(implicit ab: ActionBuilder)
      extends AbstractRuleBox {
    def toRule: Rule =
      Rule(action.toAction, law)
  }

  /** A class representing abstract bidirectional Rules. */
  final case class AbstractBiRule(biaction: AbstractBiAction,
    fwdLaw: () => Double, bwdLaw: () => Double)(
    implicit bab: BiActionBuilder)
      extends AbstractRuleBox {
    def toRule: BiRule =
      BiRule(biaction.toBiAction, fwdLaw, bwdLaw)
  }


  // -- Sugar for pattern and mixture construction. --

  /** Convert a partial abstract pattern into a pattern. */
  implicit def partialAbstractPatternToPattern(
    p: PartialAbstractPattern): Pattern = p.toPattern

  /** Convert a partial abstract pattern into a mixture. */
  implicit def partialAbstractPatternToMixture(
    p: PartialAbstractPattern): Mixture = p.toPattern.toMixture

  /** Convert a sequence of partial abstract patterns into an
    * abstract pattern.
    */
  @inline def partialAbstractPatternsToAbstractPattern(
    pps: Seq[PartialAbstractPattern]): AbstractPattern =
    pps.foldLeft(AbstractPattern())(_ ++ _.toAbstractPattern)

  /** Convert a sequence of partial abstract patterns into a pattern. */
  @inline def partialAbstractPatternsToPattern(
    pps: Seq[PartialAbstractPattern]): Pattern =
    partialAbstractPatternsToAbstractPattern(pps).toPattern

  /** Convert a sequence of partial abstract patterns into a mixture. */
  @inline def partialAbstractPatternsToMixture(
    pps: Seq[PartialAbstractPattern]): Mixture =
    partialAbstractPatternsToPattern(pps).toMixture


  // -- Sugar for action and rule construction. --

  // RHZ: No one is using these methods.

  // /** Convert abstract actions into actions. */
  // implicit def abstractActionToAction(a: AbstractAction)(
  //   implicit ab: ActionBuilder): Action = a.toAction

  // /** Convert abstract bidirectional actions into bidirectional actions. */
  // implicit def abstractBiActionToBiAction(a: AbstractBiAction)(
  //   implicit bab: BiActionBuilder): BiAction = a.toBiAction

  // /** Convert abstract rules into rules. */
  // implicit def abstractRuleToRule(r: AbstractRule): Rule = r.toRule

  // /** Convert abstract bidirectional rules into bidirectional rules. */
  // implicit def abstractBiRuleToBiRule(r: AbstractBiRule)
  //     : BiRule = r.toRule

  /**
   * Convert a sequence of partial abstract rules into a sequence of
   * rules.
   */
  def partialAbstractRulesToAbstractRules(prs: Seq[PartialAbstractRule])(
    implicit ab: ActionBuilder, bab: BiActionBuilder)
      : Seq[AbstractRuleBox] = {

    def failIncomplete(pr: PartialAbstractRule) {
      val msg = pr match {
        case _: PartialAbstractPattern => "missing right-hand side (\"-> ...\")"
        case _: AbstractAction         => "missing rate expression (\":@ ...\")"
        case _: AbstractRuleTail       => "missing left-hand side (\"... ->\")"
        case _                         => "unknown error"
      }
      throw new IllegalArgumentException(
        "error while constructing rule: " + msg)
    }

    // Iterate over all the partial rules and try to build total rules.
    val rs = new mutable.ArrayBuffer[AbstractRuleBox]()
    var pr: Option[PartialAbstractRule] = None
    prs foreach {
      case p: PartialAbstractPattern => pr match {
        case None                             => pr = Some(p)
        case Some(p1: PartialAbstractPattern) =>
          pr = Some(p1 ++ p.toAbstractPattern)
        case Some(a:  AbstractAction)         =>
          pr = Some(a ++ p.toAbstractPattern)
        case _ => failIncomplete(pr.get)
      }
      case a: AbstractAction         => pr match {
        case None                             => pr = Some(a)
        case Some(p:  PartialAbstractPattern) => pr = Some(p ++ a)
        case _ => failIncomplete(pr.get)
      }
      case a: AbstractBiAction => pr match {
        case None => pr = Some(a)
        case Some(p:  PartialAbstractPattern) => pr = Some(p ++ a)
        case _ => failIncomplete(pr.get)
      }
      case t: AbstractRuleTail       => pr match {
        case None                    => pr = Some(t)
        case Some(a: AbstractAction) => {
          rs += (a ++ t)
          pr = None
        }
        case _ => failIncomplete(pr.get)
      }
      case t: AbstractBiRuleTail       => pr match {
        case None                      => pr = Some(t)
        case Some(a: AbstractBiAction) => {
          rs += (a ++ t)
          pr = None
        }
        case _ => failIncomplete(pr.get)
      }
      case r: AbstractRule => {
        if (!pr.isEmpty) failIncomplete(pr.get)
        else rs += r
      }
      case r: AbstractBiRule => {
        if (!pr.isEmpty) failIncomplete(pr.get)
        else rs += r
      }
    }
    if (!pr.isEmpty) failIncomplete(pr.get)
    rs
  }

  def partialAbstractRulesToRules(prs: Seq[PartialAbstractRule])(
    implicit ab: ActionBuilder, bab: BiActionBuilder)
      : Seq[RuleBox] =
    partialAbstractRulesToAbstractRules(prs) map (_.toRule)
}

