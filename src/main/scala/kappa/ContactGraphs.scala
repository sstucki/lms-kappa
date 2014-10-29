package kappa

import scala.collection.mutable


trait ContactGraphs {
  this: LanguageContext
      with SiteGraphs =>

  protected val contactGraph: ContactGraph

  // def contactGraph: ContactGraph =
  //   if (_contactGraph != null) _contactGraph
  //   else throw new IllegalStateException("no contact graph defined")

  // NOTE: Perhaps a contact graph should be some kind of Map from
  // an agent unique identifier (to be defined by the language
  // extension) to the agents.  In this way it would be probably
  // easier to extend a contact graph.

  /** Contact graph. */
  final class ContactGraph extends Seq[ContactGraph.Agent] {

    import ContactGraph._

    val agents = mutable.MutableList[Agent]()

    def +=(that: Agent): ContactGraph = {
      agents find (_.states.undefinedState ==
        that.states.undefinedState) match {
        case Some(agent) => agent += that
        case None => agents += that
      }
      this
    }

    // /** TODO: Extend this contact graph. */
    // def +=(that: String) = this

    // -- Core Seq[ContactGraph.Agent] API --
    @inline def apply(idx: Int): Agent = agents(idx)
    @inline def iterator: Iterator[Agent] = agents.iterator
    @inline def length: Int = agents.length
  }

  object ContactGraph {

    final case class Link(to: Site, states: LinkStateSet)

    final class Site(val states: SiteStateSet) {

      val links = new mutable.ArrayBuffer[Link]()

      def connect(to: Site, states: LinkStateSet): Link = {
        val l = new Link(to, states)
        links += l
        l
      }
    }


    final class Agent(val states: AgentStateSet) {

      /** An iterable for the finite set of sites associated with
        * this agent.
        */
      val sites = new mutable.ArrayBuffer[Site]()

      def +=(states: SiteStateSet): Site = {
        val s = new Site(states)
        sites += s
        s
      }

      def +=(agent: Agent) {
        // TODO: I should merge the AgentStateSets as well
        for (site <- agent.sites) {
          this.sites find (_.states.undefinedState ==
            site.states.undefinedState) match {
            case Some(s) => () // TODO: and merge the SiteStateSets
            case None => sites += site
          }
        }
      }

      /** This method receives a partially defined, unordered interface
        * and returns a fully defined, ordered one. In other words, it
        * completes the given interface with the missing undefined sites.
        *
        * NOTE: The code that relies on the assumption that sites need
        * to be ordered is in Patterns.Pattern.Agent.matches.
        */
      def completeInterface(ss: Map[Site,(SiteState,SiteGraph.Link)])
          : Seq[(Site,(SiteState,SiteGraph.Link))] = {
        for (siteType <- sites) yield
          if (ss contains siteType) (siteType, ss(siteType))
          else (siteType, (siteType.states.undefinedState,
            SiteGraph.Undefined))
      }
    }
  }


  // -- Abstract syntax for contact graphs --

  /** A class representing abstract agent state sets. */
  abstract class AbstractAgentStateSet {

    /** Creates an agent state set from this abstract agent state set. */
    def toAgentStateSet: AgentStateSet
  }

  /** A class representing abstract site states. */
  abstract class AbstractSiteStateSet {

    /** Creates a site state set from this abstract site state set. */
    def toSiteStateSet: SiteStateSet
  }

  /** A trait for anything that is parsed as a link . */
  trait AbstractLinkStateSet {

    /** Creates a link state set from this abstract link state set. */
    def toLinkStateSet: LinkStateSet
  }

  final case class AbstractCG(
    agents: Vector[AbstractCGAgent] = Vector())

  final case class AbstractCGAgent(
    states: AbstractAgentStateSet,
    sites: Seq[AbstractCGSite])

  final case class AbstractCGSite(
    states: AbstractSiteStateSet,
    links: Seq[AbstractCGLink])

  final case class AbstractCGLink(
    id: LinkId,
    states: AbstractLinkStateSet)
}

