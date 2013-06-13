package kappa

import scala.language.implicitConversions
import scala.language.postfixOps

import scala.collection.mutable

// For TypeTag handling
// import scala.reflect.runtime.universe._


/**
 * Generic abstract syntax tree structures for Kappa-like languages.
 *
 * NOTE: The classes in this trait can be used as "pattern
 * combinators", but they are very different form the generic pattern
 * builder in the [[Patterns]] trait.  In fact they are more like the
 * nodes of a pattern AST, enhanced with combinator-like operators.
 */
trait AbstractSyntax {
  this: LanguageContext
      with ContactGraphs
      with Patterns
      with Mixtures
      with Actions
      with Rules
      with Parsers =>

  // -- Nodes of abstract syntax trees and their builders. --

  type EndpointName = String

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
    @inline def ->(that: Pattern)(implicit ab: ActionBuilder)
        : ab.RuleBuilder = ab(this.toPattern, that)

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
    @inline def <->(that: Pattern)(implicit bab: BiActionBuilder)
        : bab.BiRuleBuilder = bab(this.toPattern, that)

    /**
     * Build a bidirectional action from this partial abstract pattern
     * and the empty pattern.
     */
    @inline def <->()(implicit bab: BiActionBuilder): bab.BiRuleBuilder =
      bab(this.toPattern, AbstractPattern().toPattern)

    // FIXME: :@ and !@ methods should be language-specific
    /**
     * Build the tail of a rule that follows mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant
     */
    @inline def :@(rate: => Double): AbstractMassActionRuleTail =
      AbstractMassActionRuleTail(this.toAbstractPattern, () => rate)

    /**
     * Build the tail of a rule that follow an arbitrary rate law.
     *
     * @param law kinetic law expression
     */
    @inline def !@(law: => Double): AbstractRateLawRuleTail =
      AbstractRateLawRuleTail(this.toAbstractPattern, () => law)

    // TODO Add tokens?

    /**
     * Build a mixture with `x` copies of this abstract pattern.
     *
     * @param x the number of copies of this abstract pattern in the
     *        resulting mixture.
     */
    @inline def *(x: Int): Mixture = this.toMixture * x

    /** Convert this partial abstract pattern into a pattern. */
    @inline def toPattern: Pattern = toAbstractPattern.toPattern

    /** Convert this partial abstract pattern into a mixture. */
    @inline def toMixture: Mixture = Mixture(this.toPattern)
  }

  /**
   * An abstract class representing partially built abstract agents.
   *
   * Every abstract syntax node that can be converted into an agent
   * (i.e. that can be used in agent position) should extend this
   * class.
   */
  abstract class PartialAbstractAgent extends PartialAbstractPattern {

    /** Convert this partial abstract agent into an abstract agent. */
    @inline def toAbstractAgent: AbstractAgent

    /** Convert this partial abstract agent into an abstract pattern. */
    @inline def toAbstractPattern: AbstractPattern =
      toAbstractAgent.toAbstractPattern
  }

  /**
   * An abstract class representing partially built abstract sites.
   *
   * Every abstract syntax node that can be converted into a site
   * (i.e. that can be used in site position) should extend this
   * class.
   */
  abstract class PartialAbstractSite {

    /** Convert this partial abstract site into an abstract site. */
    def toAbstractSite: AbstractSite
  }


  /** A class representing abstract agent states. */
  abstract class AbstractAgentState extends PartialAbstractAgent {

    /**
     * Build an abstract agent from this partial abstract agent.
     *
     * @param sites a sequence of partial abstract sites representing
     *        the interface of the abstract agent to build.
     */
    def apply(sites: PartialAbstractSite*): AbstractAgent = {

      // Convert partial sites into total sites.
      val totalSites: Seq[AbstractSite] = sites map (_.toAbstractSite)

      // Then create the agent wrapper
      AbstractAgent(this, totalSites)
    }

    @inline def toAbstractAgent: AbstractAgent = apply()

    /** Creates an agent state from this abstract agent state. */
    def toAgentState: AgentState
  }

  /** A class representing abstract site states. */
  abstract class AbstractSiteState extends PartialAbstractSite {

    /**
     * Build an undefined abstract site from this partial abstract
     * site.
     */
    @inline def ?(): AbstractSite =
      AbstractSite(this, AbstractUndefined)

    /** Build a free abstract site from this partial abstract site. */
    @inline def !-(): AbstractSite =
      AbstractSite(this, AbstractStub)

    /**
     * Build an abstract site with a wildcard link from this partial
     * abstract site.
     *
     * @param wc the wildcard connected to this partial abstract site.
     */
    @inline def !(
      agentState: Option[AbstractAgentState],
      siteState: Option[AbstractSiteState],
      linkState: Option[AbstractLinkState]): AbstractSite =
      this ! AbstractWildcard(agentState, siteState, linkState)

    /**
     * Build an abstract site with a maximally general wildcard link
     * from this partial abstract site.
     */
    @inline def !*(): AbstractSite = this ! (None, None, None)

    /**
     * Build a linked abstract site from this partial abstract site.
     *
     * @param link the link connecting this partial abstract site.
     */
    @inline def !(link: AbstractLink): AbstractSite =
      AbstractSite(this, link)

    @inline def /(endpointName: EndpointName): AbstractSite =
      AbstractSite(this, AbstractEndpoint(endpointName))

    @inline def toAbstractSite: AbstractSite = this.!-

    /** Creates a site state from this abstract site state. */
    def toSiteState(agentStateSet: AgentStateSet): SiteState
  }

  /** A class representing abstract link states. */
  abstract class AbstractLinkState {

    // FIXME: Still storing the source and target of a link in the
    // state itself, but that information should be stored in the
    // contact graph (ie as we do with Patterns).

    /** Creates a link state from this abstract link state. */
    def toLinkState(linkId: Option[LinkId], source: SiteStateSet, target: SiteStateSet): LinkState
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


  /** A class representing abstract sites. */
  final case class AbstractSite(state: AbstractSiteState,
    link: AbstractLink) extends PartialAbstractSite {

    @inline def toAbstractSite = this
  }

  /** A class representing abstract agents. */
  final case class AbstractAgent(
    state: AbstractAgentState, sites: Seq[AbstractSite])
      extends PartialAbstractAgent {

    @inline def toAbstractAgent = this

    @inline override def toAbstractPattern: AbstractPattern =
      AbstractPattern(Vector(this))
  }

  /** A class representing abstract patterns. */
  final case class AbstractPattern(
    agents: Vector[AbstractAgent] = Vector(),
    siteGraphString: String = "",
    links: Seq[(AgentIndex, SiteIndex, AbstractLinkState,
                AgentIndex, SiteIndex, AbstractLinkState)] =
      List())
      extends PartialAbstractPattern {
    self =>

    @inline override def :+(that: AbstractAgent): AbstractPattern = {
      AbstractPattern(agents :+ that, siteGraphString, links)
    }

    @inline override def ++(that: AbstractPattern): AbstractPattern =
      AbstractPattern(this.agents ++ that.agents, siteGraphString, links)

    @inline override def +:(that: AbstractAgent): AbstractPattern = {
      AbstractPattern(that +: agents, siteGraphString, links)
    }

    // *** Connectors ***

    // RHZ: How can I add things on the language-specific side here?
    // For instance, I'd like to add a + method in Kappa that doesn't
    // require the AbstractLinkState.

    final class Connector(
      eps: List[(EndpointName, AbstractLinkState,
                 AbstractLinkState, EndpointName)]) {
      // epToLink: List[(EndpointName, AbstractLink)],
      // linkToEp: List[(AbstractLink, EndpointName)]) {

      def +(ep1: EndpointName, ls1: AbstractLinkState,
            ls2: AbstractLinkState, ep2: EndpointName) =
        new Connector((ep1, ls1, ls2, ep2) :: eps) //,
      //    epToLink, linkToEp)

      def +(xs: Seq[(EndpointName, AbstractLinkState,
                     AbstractLinkState, EndpointName)]) =
        new Connector(xs.toList ++ eps)

      // def +(ep1: EndpointName, l2: AbstractLink) =
      //   new Connector(epToEp, (ep1, l2) :: epToLink, linkToEp)

      // def +(l1: AbstractLink, ep2: EndpointName) =
      //   new Connector(epToEp, epToLink, (l1, ep2) :: linkToEp)

      // def +[T: TypeTag](xs: Seq[T]) =
      //   if (typeOf[T] <:< typeOf[(EndpointName, AbstractLinkState,
      //                             EndpointName, AbstractLinkState)])
      //     new Connector(xs.asInstanceOf[
      //       Seq[(EndpointName, AbstractLinkState,
      //            EndpointName, AbstractLinkState)]].toList ++
      //       this.epToEp, epToLink, linkToEp)
      //   else if (typeOf[T] <:< typeOf[(EndpointName, AbstractLink)])
      //     new Connector(epToEp, xs.asInstanceOf[
      //       Seq[(EndpointName, AbstractLink)]].toList ++ epToLink,
      //       linkToEp)
      //   else if (typeOf[T] <:< typeOf[(AbstractLink, EndpointName)])
      //     new Connector(epToEp, epToLink, xs.asInstanceOf[
      //       Seq[(AbstractLink, EndpointName)]].toList ++ linkToEp)
      //   else
      //     throw new IllegalArgumentException(
      //       "argument is not of appropiate type")


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
        for ((ep1, xs) <- openEndpoints(self)) eps1 += ((ep1, xs))

        val eps2: mutable.HashMap[EndpointName, Seq[(AgentIndex, SiteIndex)]] =
          new mutable.HashMap()
        for ((ep2, xs) <- openEndpoints(that)) eps2 += ((ep2, xs))

        val offset = self.agents.length

        val links = for {
          (ep1, ls1, ls2, ep2) <- eps
          ((i1, j1) +: ep1rest) <- eps1 get ep1
          ((i2, j2) +: ep2rest) <- eps2 get ep2
        } yield {
          eps1 += ((ep1, ep1rest))
          eps2 += ((ep2, ep2rest))
          (i1, j1, ls1, offset + i2, j2, ls2)
        }

        def undefineSites(
          agents: Seq[AbstractAgent],
          siteMap: Map[AgentIndex, Set[SiteIndex]])
            : Seq[AbstractAgent] =
          for ((u:AbstractAgent, i) <- agents.zipWithIndex) yield
            if (siteMap(i).nonEmpty)
              AbstractAgent(u.state,
                for ((s:AbstractSite, j) <- u.sites.zipWithIndex)
                yield if (siteMap(i) contains j)
                        AbstractSite(s.state, AbstractUndefined)
                      else s)
            else u

        val sm1 = links.foldLeft(Map() withDefaultValue
          Set(): Map[AgentIndex, Set[SiteIndex]]) {
            case (sm, (i1, j1, _, _, _, _)) =>
              sm + ((i1, sm(i1) + j1)) }

        val sm2 = links.foldLeft(Map() withDefaultValue
          Set(): Map[AgentIndex, Set[SiteIndex]]) {
            case (sm, (_, _, _, i2, j2, _)) =>
              sm + ((i2 - offset, sm(i2 - offset) + j2)) }

        val newAgents =
          undefineSites(self.agents, sm1) ++
          undefineSites(that.agents, sm2)

        val thatLinks =
          for ((i1, j1, ls1, i2, j2, ls2) <- that.links)
          yield (offset + i1, j1, ls1, offset + i2, j2, ls2)

        AbstractPattern(newAgents.toVector,
          self.siteGraphString + that.siteGraphString,
          self.links ++ thatLinks ++ links.toList)
      }
    }

    //     // Set of endpoints in `this` to be connected to endpoints
    //     // in `that`.
    //     val epToEp1 = epToEp map (_._1) toSet

    //     // Map from endpoints in `this` to abstract links
    //     val epToLink1 = epToLink.toMap

    //     // Fill the new agents array with agents from this pattern
    //     // and close the corresponding endpoints
    //     for ((u1:AbstractAgent, i1) <- self.agents.zipWithIndex) {
    //       val sites = for ((s1:AbstractSite, j1) <- u1.sites.zipWithIndex) yield s1.link match {
    //         case AbstractEndpoint(ep1) =>
    //           // if s1.link is an open endpoint and it has to
    //           // be connected with another open endpoint...
    //           if (epToEp1 contains ep1) {
    //             // then we'll register ep1 in linkMap
    //             linkMap += ((ep1, i1, j1))
    //             // and return a site with a closed endpoint
    //             AbstractSite(s1.state, AbstractUndefined)

    //           } else if (epToLink1 contains ep1) {
    //             // instead if it's an open endpoint that has to be
    //             // replaced by an abstract link, we do that
    //             AbstractSite(s1.state, epToLink1(ep1))

    //           } else {
    //             s1 // if s1.link is an open endpoint but we are going
    //                // to connect it, we leave it open
    //           }
    //         case _ => s1 // if s1.link is not an open endpoint,
    //                      // we leave it like that
    //       }
    //       // add the new abstract agent
    //       newAgents(i1) = AbstractAgent(u1.state, sites)
    //     }

    //     val offset = self.agents.length

    //     // Map from the endpoints in `that` to the endpoints in `this`
    //     // plus the abstract link states for the new link
    //     val epToEp2 = (for ((ep1, ls1, ep2, ls2) <- epToEp)
    //                    yield (ep2, (ep1, ls1, ls2))).toMap

    //     // Map from endpoints in `that` to abstract links
    //     val epToLink2 = linkToEp map (_.swap) toMap

    //     for ((u2, i2) <- that.agents.zipWithIndex) {
    //       val sites = for ((s2, j2) <- u2.sites.zipWithIndex) yield {
    //         s2.link match {
    //           case AbstractEndpoint(ep2) =>
    //             if (epToEp2 contains ep2) {
    //               // we add the new link to links
    //               val (ep1, ls1, ls2) = epToEp2(ep2)
    //               linkMap find (_._1 == ep1) match {
    //                 case Some((ep1, i1, j1)) =>
    //                   links += ((i1, j1, ls1, offset + i2, j2, ls2))
    //                 case None =>
    //                   throw new IllegalArgumentException(
    //                     "endpoint '" + ep1 + "' was not registered")
    //               }

    //               // and we return a site with a closed endpoint
    //               AbstractSite(s2.state, AbstractUndefined)

    //             } else if (epToLink2 contains ep2) {
    //               AbstractSite(s2.state, epToLink2(ep2))

    //             } else {
    //               s2 // if s2.link is an open endpoint but we are going
    //                  // to connect it, we leave it open
    //             }
    //           case _ => s2 // if s2.link is not an open endpoint,
    //                        // we leave it like that
    //         }
    //       }
    //       newAgents(offset + i2) = AbstractAgent(u2.state, sites)
    //     }

    //     val thatLinks = 
    //       for ((i1, j1, ls1, i2, j2, ls2) <- that.links)
    //       yield (i1, j1, ls1, offset + i2, j2, ls2)

    //     AbstractPattern(newAgents.toVector,
    //       self.siteGraphString + that.siteGraphString,
    //       links.toList ++ self.links ++ thatLinks)
    //   }
    // }

    /** Connect this abstract pattern (say `p1`) to another abstract
      * pattern (say `p2`) using the open endpoint `ep1` in `p1` and
      * `ep2` in `p2`.  The resulting link has states `ls1`, `ls2`.
      */
    def -<(ep1: EndpointName, ls1: AbstractLinkState,
           ls2: AbstractLinkState, ep2: EndpointName) =
      new Connector(List((ep1, ls1, ls2, ep2))) //, List(), List())

    def -<(xs: Seq[(EndpointName, AbstractLinkState,
                    AbstractLinkState, EndpointName)]) =
      new Connector(xs.toList)

    // /** Connect this abstract pattern (say `p1`) to another abstract
    //   * pattern (say `p2`) by replacing a given open endpoint `ep1`
    //   * in `p1` for a given abstract link `l1`.
    //   */
    // def -<(ep1: EndpointName, l1: AbstractLink) =
    //   new Connector(List(), List((ep1, l1)), List())

    // /** Connect this abstract pattern (say `p1`) to another abstract
    //   * pattern (say `p2`) by replacing a given open endpoint `ep2`
    //   * in `p2` for a given abstract link `l2`.
    //   */
    // def -<(l2: AbstractLink, ep2: EndpointName) =
    //   new Connector(List(), List(), List((l2, ep2)))

    // /** Connect this abstract pattern (say `p1`) to another abstract
    //   * pattern (say `p2`) using a list of connector instructions.
    //   * A connector instruction can be any of:
    //   * 1. A tuple (ep1, ls1, ep2, ls2): Connect open endpoint `ep1`
    //   * in `p1` and `ep2` in `p2` with link states `ls1`, `ls2`.
    //   * 2. A tuple (ep1, l1): Replace open endpoint `ep1` in `p1` by
    //   * `l1`.
    //   * 3. A tuple (ep2, l2): Replace open endpoint `ep2` in `p2` by
    //   * `l2`.
    //   */
    // def -<[T: TypeTag](xs: Seq[T]) =
    //   if (typeOf[T] <:< typeOf[(EndpointName, AbstractLinkState,
    //                             EndpointName, AbstractLinkState)])
    //     new Connector(xs.asInstanceOf[
    //       Seq[(EndpointName, AbstractLinkState,
    //            EndpointName, AbstractLinkState)]].toList,
    //       List(), List())
    //   else if (typeOf[T] <:< typeOf[(EndpointName, AbstractLink)])
    //     new Connector(List(),
    //       xs.asInstanceOf[Seq[(EndpointName, AbstractLink)]].toList,
    //       List())
    //   else if (typeOf[T] <:< typeOf[(AbstractLink, EndpointName)])
    //     new Connector(List(), List(),
    //       xs.asInstanceOf[Seq[(AbstractLink, EndpointName)]].toList)
    //   else
    //     throw new IllegalArgumentException(
    //       "argument is not of appropiate type")

    /** Iterates over all open endpoints in this abstract pattern and
      * replace them using `f`.
      */
    def closeWith(f: EndpointName => AbstractLink): AbstractPattern = {
      val newAgents =
        for (u <- agents)
        yield AbstractAgent(u.state,
          for (s <- u.sites) yield s.link match {
            case AbstractEndpoint(ep) => AbstractSite(s.state, f(ep))
            case _ => s
          })
      AbstractPattern(newAgents.toVector, siteGraphString, links)
    }

    /** Replace all open endpoints by stubs (i.e. free sites) in this
      * abstract pattern.
      */
    def close: AbstractPattern = closeWith { _ => AbstractStub }

    // /** Get a sequence of all open endpoints in this abstract pattern. */
    // private def openEndpoints(ap: AbstractPattern): Seq[EndpointName] = {
    //   for (u <- ap.agents; s <- u.sites) yield s.link match {
    //     case AbstractEndpoint(ep1) => Some(ep1)
    //     case _ => None
    //   }
    // }.flatten

    // /** Connect `this` abstract pattern with `that` abstract pattern
    //   * using `f`.  This is done by taking the first open endpoints
    //   * in `this` and `that` and then using `f` to get two abstract
    //   * links that replace the two open endpoints.  This process is
    //   * repeated for all zipped pairs of open endpoints in `this` and
    //   * `that`.
    //   */
    // def zipWith(that: AbstractPattern)(
    //   f: (EndpointName, EndpointName) => (AbstractLink, AbstractLink))
    //     : AbstractPattern = {
    //   val ep1s = openEndpoints(this)
    //   val ep2s = openEndpoints(that)
    //   val (l1s, l2s) = ((ep1s, ep2s).zipped map f).unzip
    //   this -< (ep1s zip l2s) + (l1s zip ep2s) >- that
    // }

    // /** Connect all open endpoints in `this` abstract pattern with all
    //   * open endpoints in `that` abstract pattern using `f` to
    //   * generate the appropriate abstract link states.
    //   */
    // def zip(that: AbstractPattern)(
    //   f: (EndpointName, EndpointName) => (AbstractLinkState, AbstractLinkState))
    //     : AbstractPattern = {
    //   val ep1s = openEndpoints(this)
    //   val ep2s = openEndpoints(that)
    //   val (ls1s, ls2s) = ((ep1s, ep2s).zipped map f).unzip
    //   val epToEp =
    //     for ((((ep1, ls1), ep2), ls2) <- ep1s zip ls1s zip ep2s zip ls2s)
    //     yield (ep1, ls1, ep2, ls2)
    //   this -< epToEp >- that
    // }

    // final class Zipped(that: AbstractPattern) {
    //   def using(f: (EndpointName, EndpointName) =>
    //     (AbstractLinkState, AbstractLinkState)): AbstractPattern = {
    //     val ep1s = openEndpoints(self)
    //     val ep2s = openEndpoints(that)
    //     val (ls1s, ls2s) = ((ep1s, ep2s).zipped map f).unzip
    //     val epToEp =
    //       for ((((ep1, ls1), ep2), ls2) <- ep1s zip ls1s zip ep2s zip ls2s)
    //       yield (ep1, ls1, ep2, ls2)
    //     self -< epToEp >- that
    //   }
    // }

    // /** Connect all open endpoints in `this` abstract pattern with all
    //   * open endpoints in `that` abstract pattern using `f` to
    //   * generate the appropriate abstract link states.
    //   */
    // def zip(that: AbstractPattern) = new Zipped(that)


    @inline override def toAbstractPattern: AbstractPattern = this

    override def toPattern: Pattern = {

      // Create a builder to build this pattern
      val pb = new Pattern.Builder(siteGraphString)

      // Create a map to track which sites a given link connects.
      type LinkMapValue = List[(pb.type#Agent#Site, AbstractLinkState)]
      val linkMap =
        new mutable.HashMap[LinkId, LinkMapValue]() withDefaultValue Nil

      // Create agents
      for (u <- agents) {

        // Create the concrete agent state from the abstract one and
        // get its associated agent state set.
        val agentState = u.state.toAgentState
        val agentStateSet = agentState.agentStateSet

        // Create the concrete site states and complete the interface
        // by filling in undefined sites states.
        val sites = { for (s <- u.sites) yield
          (s.state.toSiteState(agentStateSet), s)
        }.toMap

        val siteStates = agentStateSet.completeInterface(sites.keys)
        val intf = siteStates map { siteState => (siteState,
          sites.get(siteState) map (_.link) getOrElse AbstractUndefined) }

        val v = pb += agentState
        for ((siteState, link) <- intf) {

          val x = v += siteState

          link match {
            case AbstractUndefined => ()

            case AbstractStub =>
              x define Pattern.Builder.Stub

            case AbstractWildcard(aas, ass, als) => {
              // a{a,s,l}s: abstract {agent,site,link} state
              val as = for (aas <- aas) yield aas.toAgentState
              val ss = for (as <- as; ass <- ass) yield
                ass.toSiteState(as.agentStateSet)
              val ls = for (ss <- ss; als <- als) yield
                als.toLinkState(None, siteState.siteStateSet,
                  ss.siteStateSet)
              x define Pattern.Builder.Wildcard(as, ss, ls)
            }

            case l: AbstractLinked =>
              linkMap += ((l.id, (x, l.state) :: linkMap(l.id)))

            case AbstractEndpoint(ep) =>
              throw new IllegalStateException(
                "abstract pattern still has open endpoints (e.g. " + ep + ")")
          }
        }
      }

      // Connect links
      for (l <- linkMap) l match {
        case (id, List((s1, l1), (s2, l2))) => {
          val t1 = s1.state.siteStateSet
          val t2 = s2.state.siteStateSet
          val ls1 = l1.toLinkState(Some(id), t1, t2)
          val ls2 = l2.toLinkState(Some(id), t2, t1)
          s1 connect (s2, ls1, ls2)
        }
        case (_, List()) => ()
        case (id, List(_)) => throw new IllegalStateException(
          "dangling link with label " + id)
        case (id, xs) => throw new IllegalStateException(
          "attempt to create hyperlink with label " + id +
          " to connect sites: " + (xs map (_._1.state)).mkString(", "))
      }

      // Connect open endpoints
      for ((i1, j1, ls1, i2, j2, ls2) <- links) {
        val u1 = pb.agents(i1)
        val u2 = pb.agents(i2)
        val x1 = u1.sites(j1)
        val x2 = u2.sites(j2)
        val ss1 = x1.state.siteStateSet
        val ss2 = x2.state.siteStateSet
        x1 connect (x2,
          ls1.toLinkState(None, ss1, ss2),
          ls2.toLinkState(None, ss2, ss1))
      }

      // Build the pattern
      pb.build
    }
  }

  /** A class representing abstract actions. */
  final case class AbstractAction(
    lhs: AbstractPattern, rhs: AbstractPattern) extends PartialAbstractRule {

    /** Append an abstract agent to this abstract action.  */
    @inline def :+(that: AbstractAgent): AbstractAction =
      AbstractAction(lhs, rhs :+ that)

    /** Append an abstract pattern to this abstract action.  */
    @inline def ++(that: AbstractPattern): AbstractAction =
      AbstractAction(lhs, rhs ++ that)

    /** Append an abstract rule tail to this abstract action.  */
    @inline def ++(that: AbstractRuleTail)(
      implicit ab: ActionBuilder): AbstractRule = {
      val action = AbstractAction(lhs, this.rhs ++ that.rhs).toAction
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
     * @param rate stochastic kinetic rate constant
     */
    def :@(rate: => Double)(implicit ab: ActionBuilder): AbstractRule = {
      val action = toAction
      AbstractRule(action, () => action.lhs.inMix * rate)
    }

    /**
     * Build a rule that follow an arbitrary rate law.
     *
     * @param law kinetic law expression
     */
    def !@(law: => Double)(implicit ab: ActionBuilder): AbstractRule =
      AbstractRule(toAction, () => law)

    /** Convert this abstract action into an action. */
    @inline def toAction()(implicit ab: ActionBuilder): Action =
      ab(lhs.toPattern, rhs.toPattern).getAction
  }

  /** A class representing abstract bidirectional actions. */
  final case class AbstractBiAction(
    lhs: AbstractPattern, rhs: AbstractPattern) extends PartialAbstractRule {

    /** Append an abstract agent to this abstract bidirectional action.  */
    @inline def :+(that: AbstractAgent): AbstractBiAction =
      AbstractBiAction(lhs, rhs :+ that)

    /** Append an abstract pattern to this abstract bidirectional action.  */
    @inline def ++(that: AbstractPattern): AbstractBiAction =
      AbstractBiAction(lhs, rhs ++ that)

    /** Append an abstract rule tail to this abstract bidirectional action.  */
    @inline def ++(that: AbstractBiRuleTail)(
      implicit bab: BiActionBuilder): AbstractBiRule = {
      val action = AbstractBiAction(lhs, this.rhs ++ that.rhs).toBiAction
      AbstractBiRule(action, that.toFwdRate(action.lhs), that.toBwdRate(action.rhs))
    }

    /** Prepend an abstract agent to this abstract bidirectional action.  */
    @inline def +:(that: AbstractAgent): AbstractBiAction =
      AbstractBiAction(that +: lhs, rhs)

    /** Prepend an abstract agent to this abstract bidirectional action.  */
    @inline def ::(that: AbstractAgent): AbstractBiAction =
      that +: this

    // FIXME: :@ and !@ methods should be language-specific
    /**
     * Build a bidirectional rule that follows mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant
     */
    def :@(fwdRate: => Double, bwdRate: => Double)(
      implicit bab: BiActionBuilder): AbstractBiRule = {
      val biaction = toBiAction
      AbstractBiRule(biaction, () => biaction.lhs.inMix * fwdRate,
        () => biaction.rhs.inMix * bwdRate)
    }

    /**
     * Build a bidirectional rule that follow an arbitrary rate law.
     *
     * @param law kinetic law expression
     */
    def !@(fwdLaw: => Double, bwdLaw: => Double)(
      implicit bab: BiActionBuilder): AbstractBiRule =
      AbstractBiRule(toBiAction, () => fwdLaw, () => bwdLaw)

    /** Convert this abstract bidirectional action into a bidirectional action. */
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
    @inline def ::(that: AbstractAgent): AbstractRuleTail = that +: this
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
    @inline def toRate(lhs: Pattern): () => Double = () => lhs.inMix * rate()
  }

  /** A class representing tails of rules that follow arbitrary rate laws. */
  final case class AbstractRateLawRuleTail(
    rhs: AbstractPattern, law: () => Double) extends AbstractRuleTail {

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
    @inline def ::(that: AbstractAgent): AbstractBiRuleTail = that +: this
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

  /** A class representing abstract Rules. */
  final case class AbstractRule(action: Action, law: () => Double)
      extends PartialAbstractRule {
    def toRule: Rule = Rule(action, law)
  }

  /** A class representing abstract bidirectional Rules. */
  final case class AbstractBiRule(biaction: BiAction,
    fwdLaw: () => Double, bwdLaw: () => Double)
      extends PartialAbstractRule {
    def toRule: BiRule = BiRule(biaction, fwdLaw, bwdLaw)
  }


  // -- Sugar for pattern and mixture construction. --

  /** Convert a partial abstract pattern into a pattern. */
  implicit def partialAbstractPatternToPattern(
    p: PartialAbstractPattern): Pattern = p.toPattern

  /** Convert a partial abstract pattern into a mixture. */
  implicit def partialAbstractPatternToMixture(
    p: PartialAbstractPattern): Mixture = Mixture(p.toPattern)

  /** Convert a sequence of partial abstract patterns into a pattern. */
  @inline def partialAbstractPatternsToPattern(
    pps: Seq[PartialAbstractPattern]): Pattern =
    pps.foldLeft(AbstractPattern())(_ ++ _.toAbstractPattern).toPattern

  /** Convert a sequence of partial abstract patterns into a mixture. */
  @inline def partialAbstractPatternsToMixture(
    pps: Seq[PartialAbstractPattern]): Mixture =
    Mixture(partialAbstractPatternsToPattern(pps))

  /**
   * Build a pattern from a string.
   *
   * This method invokes the [[Parser]] to parse an expression.  It
   * then walks the [[Parser.AST]] and builds a [[Patterns.Pattern]]
   * from the expression using the operators of the abstract syntax
   * classes.
   *
   * @param expr the string to build the pattern from.
   * @return a pattern corresponding to the expression `expr`.
   */
  implicit def stringToPattern(expr: String): Pattern =
    parseSiteGraph(expr).toPattern

  /**
   * Build a mixture from a string.
   *
   * This method first builds a [[Patterns#Pattern]] from a string and
   * subsequently converts it into a [[Mixtures#Mixture]].
   *
   * @param expr the string to build the mixture from.
   * @return a mixture corresponding to the expression `expr`.
   */
  implicit def stringToMixture(expr: String) = Mixture(stringToPattern(expr))

  implicit def scToInterpolator(sc: StringContext): Interpolator =
    new Interpolator(sc)

  final class Interpolator(sc: StringContext) {
    def p(args: Any*): Pattern = stringToPattern( sc.s(args :_*) )
    def m(args: Any*): Mixture = stringToMixture( sc.s(args :_*) )
  }


  // -- Sugar for action and rule construction. --

  /** Convert abstract actions into actions. */
  implicit def abstractActionToAction(a: AbstractAction)(
    implicit ab: ActionBuilder): Action = a.toAction

  /** Convert abstract bidirectional actions into bidirectional actions. */
  implicit def abstractBiActionToBiAction(a: AbstractBiAction)(
    implicit bab: BiActionBuilder): BiAction = a.toBiAction

  // TODO Implicits conversions are too fragile
  /**
   * Convert a pair `(lhs, rhs)` of site graph expressions into an
   * action.
   */
  implicit def stringPairToAction(lr: (String, String))(
    implicit ab: ActionBuilder): ab.RuleBuilder =
    ab(stringToPattern(lr._1), stringToPattern(lr._2))

  /** Convert abstract rules into rules. */
  implicit def abstractRuleToRule(r: AbstractRule): Rule = r.toRule

  /** Convert abstract bidirectional rules into bidirectional rules. */
  implicit def abstractBiRuleToBiRule(r: AbstractBiRule)
      : BiRule = r.toRule

  /** Convert an abstract rule to a rule and register it. */
  def registerRule(r: AbstractRule) { registerRule(r.toRule) }

  /** Convert an abstract rule to a rule and register it. */
  def registerBiRule(r: AbstractBiRule) { registerRule(r.toRule) }

  /**
   * Convert a sequence of partial abstract rules into a sequence of
   * rules.
   */
  def partialAbstractRulesToRules(prs: Seq[PartialAbstractRule])(
    implicit ab: ActionBuilder, bab: BiActionBuilder)
      : Seq[RuleBox] = {

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
    val rs = new mutable.ArrayBuffer[RuleBox]()
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
          rs += (a ++ t).toRule
          pr = None
        }
        case _ => failIncomplete(pr.get)
      }
      case t: AbstractBiRuleTail       => pr match {
        case None                      => pr = Some(t)
        case Some(a: AbstractBiAction) => {
          rs += (a ++ t).toRule
          pr = None
        }
        case _ => failIncomplete(pr.get)
      }
      case r: AbstractRule => {
        if (!pr.isEmpty) failIncomplete(pr.get)
        else rs += r.toRule
      }
      case r: AbstractBiRule => {
        if (!pr.isEmpty) failIncomplete(pr.get)
        else rs += r.toRule
      }
    }
    if (!pr.isEmpty) failIncomplete(pr.get)
    rs
  }
}

