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
    @inline def :+(that: PartialAbstractAgent): AbstractPattern =
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
    @inline def +:(that: PartialAbstractAgent): AbstractPattern =
      that +: this.toAbstractPattern

    /** Prepend an abstract action to this partial abstract pattern.  */
    @inline def +:(that: AbstractAction): AbstractAction =
      that ++ this.toAbstractPattern

    /** Prepend an abstract bidirectional action to this partial abstract pattern.  */
    @inline def +:(that: AbstractBiAction): AbstractBiAction =
      that ++ this.toAbstractPattern

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def ::(that: PartialAbstractAgent): AbstractPattern =
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
    @inline def ->(that: PartialAbstractAgent): AbstractAction =
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
    @inline def <->(that: PartialAbstractAgent): AbstractBiAction =
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

  /**
   * An abstract class representing partially built abstract agents.
   *
   * Every abstract syntax node that can be converted into an agent
   * (i.e. that can be used in agent position) should extend this
   * class.
   */
  abstract class PartialAbstractAgent extends PartialAbstractPattern {

    type State <: AbstractAgentState

    val agentType: ContactGraph.Agent

    /** An abstract class representing partially built abstract sites.
      *
      * Every abstract syntax node that can be converted into a site
      * (i.e. that can be used in site position) should extend this
      * class.
      */
    abstract class PartialAbstractSite {

      type State <: AbstractAgentState#AbstractSiteState

      val siteType: agentType.Site

      /** Convert this partial abstract site into an abstract site. */
      def toAbstractSite: AbstractSite[State]
    }

    /** Convert this partial abstract agent into an abstract agent. */
    def toAbstractAgent: AbstractAgent[State]

    /** Convert this partial abstract agent into an abstract pattern. */
    @inline def toAbstractPattern: AbstractPattern =
      this.toAbstractAgent.toAbstractPattern
  }


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
    agntState: Option[AbstractAgentState],
    siteState: Option[AbstractAgentState#AbstractSiteState],
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


  // - Agents and Sites -

  /** A class representing abstract agent states. */
  abstract class AbstractAgentState extends PartialAbstractAgent {

    type State = this.type

    /**
     * Build an abstract agent from this partial abstract agent.
     *
     * @param sites a sequence of partial abstract sites representing
     *        the interface of the abstract agent to build.
     */
    def apply(sites: AbstractSiteState*): AbstractAgent[State] = {
      // Convert partial sites into total sites and
      // put them into an AbstractAgent with `this` state
      AbstractAgent(agentType, this)(sites map (_.toAbstractSite))
    }

    @inline override def toAbstractAgent = apply()

    /** Creates an agent state from this abstract agent state. */
    def toAgentState: AgentState


    /** A class representing abstract site states. */
    abstract class AbstractSiteState extends PartialAbstractSite {

      type State = this.type

      /** Build an undefined abstract site from this partial abstract
        * site.
        */
      @inline def ?() = AbstractSite[State](this, AbstractUndefined)

      /** Build a free abstract site from this partial abstract site. */
      @inline def !-() = AbstractSite[State](this, AbstractStub)

      /** Build an abstract site with a wildcard link from this partial
        * abstract site.
        *
        * @param wc the wildcard connected to this partial abstract site.
        */
      @inline def !(
        agntState: Option[AbstractAgentState],
        siteState: Option[AbstractAgentState#AbstractSiteState],
        linkState: Option[AbstractLinkState])
          : AbstractSite[State] =
        this ! AbstractWildcard(agntState, siteState, linkState)

      // TODO: Is it possible to have two methods `!` for wildcards,
      // one like the one below and the other one that accepts a
      // siteState only if an agentState has been provided and where
      // the type of siteState is agentState.AbstractSiteState?
      //
      // def !(agentState: None, linkState: Option[AbstractLinkState]) =
      //   this ! AbstractWildcard(None, None, linkState)

      /** Build an abstract site with a maximally general wildcard link
        * from this partial abstract site.
        */
      @inline def !*() = this ! (None, None, None)

      /** Build a linked abstract site from this partial abstract site.
        *
        * @param link the link connecting this partial abstract site.
        */
      @inline def !(link: AbstractLink) =
        AbstractSite[State](this, link)

      @inline def /(endpointName: EndpointName) =
        AbstractSite[State](this, AbstractEndpoint(endpointName))

      @inline override def toAbstractSite = this.!-

      /** Creates a site state from this abstract site state. */
      def toSiteState: SiteState
    }
  }

  // /** A class representing abstract sites. */
  // FIXME: AbstractSite doesn't have ! and / methods
  final case class AbstractSite[+S <:
    AbstractAgentState#AbstractSiteState](
    state: S,
    link: AbstractLink)
  //     extends PartialAbstractSite {
  //   @inline override def toAbstractSite = this
  // }

  /** A class representing abstract agents. */
  abstract class AbstractAgent[+S <: AbstractAgentState](
    val agentType: ContactGraph.Agent,
    val state: S)
      extends PartialAbstractAgent {

    type State = state.type
    type Site = AbstractSite[state.AbstractSiteState]

    val sites: Seq[Site]
    // val siteTypes: Seq[agentType.Site]
    // val siteStates: Seq[state.AbstractSiteState]
    // val links: Seq[AbstractLink]

    def undefineSites(siteIndices: Set[SiteIndex])
     // : AbstractAgent[State] =
        : AbstractAgent[S] =
      // if (siteIndices.isEmpty) this
      // else AbstractAgent(agentType, state) {
      //   val siteTypes = this.siteTypes
      //   val siteStates = this.siteStates
      //   val links = for ((l, i) <- this.links.zipWithIndex) yield
      //     if (siteIndices contains i) AbstractUndefined else l
      // }
      if (siteIndices.isEmpty) this
      else AbstractAgent(agentType, state)(
        for ((s, i) <- sites.zipWithIndex) yield
          if (siteIndices contains i)
            AbstractSite(s.state, AbstractUndefined)
          else s)

    def closeWith(n: Int, f: EndpointName => AbstractLink)
        : (AbstractAgent[state.type], Int) = {

      val (_sites: List[Site], m) = sites.foldLeft(
        (List[Site](), n))({
          case ((sites, n), AbstractSite(state, AbstractEndpoint(ep)))
              if n > 0 => (AbstractSite(state, f(ep)) :: sites, n-1)
          case ((sites, n), s) => (s :: sites, n)
        })
      (AbstractAgent(agentType, state)(_sites.reverse), m)
    }

    def addToPattern(pb: Pattern.Builder) {
      pb.addAgent(state.agentType, state.toAgentState)(
        for (s <- sites) yield (s.state.siteType,
          s.state.toSiteState))
    }
  }

  object AbstractAgent {
    def apply(agentType: ContactGraph.Agent,
      state: AbstractAgentState)(
      _sites: Seq[AbstractSite[state.AbstractSiteState]]) =
      new AbstractAgent[state.type](agentType, state) {

        val sites = _sites

        @inline override def toAbstractAgent = this

        @inline override def toAbstractPattern: AbstractPattern =
          AbstractPattern(Vector(this))
      }
  }


  // - Patterns -

  // TODO: Make this class abstract
  /** A class representing abstract patterns. */
  final case class AbstractPattern(
    agents: Vector[AbstractAgent[AbstractAgentState]] = Vector(),
    links: Seq[(AgentIndex, SiteIndex, AbstractLinkState,
                AgentIndex, SiteIndex, AbstractLinkState)] = List())
      extends PartialAbstractPattern {
    pattern =>

    type Agent = AbstractAgent[AbstractAgentState]

    @inline def :+(that: Agent): AbstractPattern =
      AbstractPattern(agents :+ that, links)

    @inline def +:(that: Agent): AbstractPattern =
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

        def undefineSites(agents: Seq[Agent],
          siteMap: Map[AgentIndex, Set[SiteIndex]]): Seq[Agent] =
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
        (Vector[Agent](), n))({
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
          val x = v._sites(j)

          // if (sites contains siteState)
          //   siteMap += ((i, sites(siteState)._2) -> x)
          siteMap += ((i, j) -> x)

          s.link match {
            case AbstractUndefined => ()

            case AbstractStub =>
              x define SiteGraph.Stub

            case AbstractWildcard(agntState, siteState, linkState) => {
              // TODO: Perhaps I should not care about link types
              // source and target...
              lazy val set = (for {
                to <- siteState map (_.siteType)
                linkType <- x.siteType.links find {
                  case ContactGraph.Link(cgTo, states) => to == cgTo }
              } yield linkType.states) getOrElse {
                throw new IllegalArgumentException("link not found")
              }
              x define SiteGraph.Wildcard(
                agntState map (_.toAgentState),
                siteState map (_.toSiteState),
                linkState map (_.toLinkState(set, None)))
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
          x1 connect (x2, lt1, lt2,
            l1.state.toLinkState(lt1.states, Some(id)),
            l2.state.toLinkState(lt2.states, Some(id)))
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
        x1 connect (x2, lt1, lt2,
          l1.toLinkState(lt1.states, None), //, ss1, ss2),
          l2.toLinkState(lt2.states, None)) //, ss2, ss1))
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
    @inline def :+(that: PartialAbstractAgent): AbstractAction =
      AbstractAction(lhs, rhs :+ that.toAbstractAgent)

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
    @inline def +:(that: PartialAbstractAgent): AbstractAction =
      AbstractAction(that.toAbstractAgent +: lhs, rhs)

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def ::(that: PartialAbstractAgent): AbstractAction =
      that.toAbstractAgent +: this

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
    @inline def :+(that: PartialAbstractAgent): AbstractBiAction =
      AbstractBiAction(lhs, rhs :+ that.toAbstractAgent)

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
    @inline def +:(that: PartialAbstractAgent): AbstractBiAction =
      AbstractBiAction(that.toAbstractAgent +: lhs, rhs)

    /** Prepend an abstract agent to this abstract bidirectional action. */
    @inline def ::(that: PartialAbstractAgent): AbstractBiAction =
      that.toAbstractAgent +: this

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
    def +:(that: PartialAbstractAgent): AbstractRuleTail

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractRuleTail

    /** Build a final rate law expression given a left-hand side. */
    def toRate(lhs: Pattern): () => Double

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def ::(that: PartialAbstractAgent): AbstractRuleTail =
      that.toAbstractAgent +: this
  }

  /** A class representing tails of rules that follow mass-action kinetics. */
  final case class AbstractMassActionRuleTail(
    rhs: AbstractPattern, rate: () => Double) extends AbstractRuleTail {

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def +:(that: PartialAbstractAgent): AbstractRuleTail =
      AbstractMassActionRuleTail(that.toAbstractAgent +: rhs, rate)

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
    @inline def +:(that: PartialAbstractAgent): AbstractRuleTail =
      AbstractRateLawRuleTail(that.toAbstractAgent +: rhs, law)

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
    def +:(that: PartialAbstractAgent): AbstractBiRuleTail

    /** Prepend an abstract pattern to the right-hand side of this tail. */
    def prependAbstractPattern(that: AbstractPattern): AbstractBiRuleTail

    /** Build a final forward rate law expression given a left-hand side. */
    def toFwdRate(lhs: Pattern): () => Double

    /** Build a final backward rate law expression given a left-hand side. */
    def toBwdRate(lhs: Pattern): () => Double

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def ::(that: PartialAbstractAgent): AbstractBiRuleTail =
      that.toAbstractAgent +: this
  }

  /** A class representing tails of bidirectional rules that follow mass-action kinetics. */
  final case class AbstractMassActionBiRuleTail(
    rhs: AbstractPattern, fwdRate: () => Double, bwdRate: () => Double)
      extends AbstractBiRuleTail {

    /** Prepend an agent to the right-hand side of this tail. */
    @inline def +:(that: PartialAbstractAgent): AbstractBiRuleTail =
      AbstractMassActionBiRuleTail(that.toAbstractAgent +: rhs,
        fwdRate, bwdRate)

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
    @inline def +:(that: PartialAbstractAgent): AbstractBiRuleTail =
      AbstractRateLawBiRuleTail(that.toAbstractAgent +: rhs,
        fwdLaw, bwdLaw)

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

