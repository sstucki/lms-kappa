package kappa

import scala.collection.mutable

import scala.language.implicitConversions


/**
 * Generic abstract syntax tree structures for Kappa-like languages.
 *
 * NOTE: The classes in this trait can be used as "pattern
 * combinators", but they are very different form the generic pattern
 * builder in the [[Patterns]] trait.  In fact they are more like the
 * nodes of a pattern AST, enhanced with combinator-like operators.
 *
 * FIXME: These classes should probably be merged with the
 * [[Parser.AST]] classes (or vice-versa) for consistency.
 */
trait AbstractSyntax {
  this: LanguageContext
      with ContactGraphs
      with Patterns
      with Mixtures
      with Actions
      with Parser =>

  // -- Nodes of abstract syntax trees and their builders. --

  /**
   * An abstract class representing partially built abstract patterns.
   *
   * Every abstract syntax node that can be converted into a pattern
   * (i.e. that can be used in pattern position) should extend this
   * class.
   */
  abstract class PartialAbstractPattern {

    /** Convert this partial abstract pattern into an abstract pattern. */
    def toAbstractPattern: AbstractPattern

    /** Append an abstract agent to this partial abstract pattern.  */
    @inline def :+(that: AbstractAgent): AbstractPattern =
      this.toAbstractPattern :+ that

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def +:(that: AbstractAgent): AbstractPattern =
      that +: this.toAbstractPattern

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def ::(that: AbstractAgent): AbstractPattern =
      that +: this

    /** Build an action from this partial pattern and a pattern. */
    @inline def ->(that: Pattern)(implicit ab: ActionBuilder): Action =
      ab(this.toPattern, that)

    /** Convert this partial abstract pattern into a pattern. */
    @inline def toPattern: Pattern = toAbstractPattern.toPattern
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

  /**
   * An abstract class representing partially built abstract links.
   *
   * Every abstract syntax node that can be converted into a link
   * (i.e. that can be used in link position) should extend this
   * class.
   */
  abstract class PartialAbstractLink {

    /** Convert this partial abstract link into an abstract link. */
    def toAbstractLink: AbstractLink
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

      // Create the concrete agent state from the abstract one and get
      // its associated agent state set.
      val as = toAgentState
      val asSet = as.agentStateSet

      // Create the concrete site states and complete the interface by
      // filling in undefined sites states.
      val linkMap = {
        for (s <- totalSites) yield (s.state.toSiteState(asSet), s.link)
      }.toMap withDefaultValue AbstractUndefined
      val allSiteStates = asSet.completeInterface(linkMap.keys)
      val intf = allSiteStates map { s => (s, linkMap(s)) }

      // Then create the agent wrapper
      AbstractAgent(as, intf)
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
     * Build a linked abstract site from this partial abstract site.
     *
     * @param name the name of the link connecting this partial
     *        abstract site.
     * @param state the state of the link connecting this partial
     *        abstract site.
     */
    @inline def !(name: LinkId, state: AbstractLinkState): AbstractSite =
      this ! AbstractLinked(name, state)

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
    @inline def !(link: PartialAbstractLink): AbstractSite =
      AbstractSite(this, link.toAbstractLink)

    @inline def toAbstractSite: AbstractSite = this.!-

    /** Creates a site state from this abstract site state. */
    def toSiteState(agentStateSet: AgentStateSet): SiteState
  }

  /** A class representing abstract link states. */
  abstract class AbstractLinkState extends PartialAbstractLink {

    // FIXME: Still storing the source and target of a link in the
    // state itself, but that information should be stored in the
    // contact graph (ie as we do with Patterns).

    /** Creates a link state from this abstract link state. */
    def toLinkState(source: SiteStateSet, target: SiteStateSet): LinkState
  }


  /** A class representing abstract links. */
  sealed abstract class AbstractLink extends PartialAbstractLink {
    @inline def toAbstractLink: AbstractLink = this
  }

  /** A class representing undefined links at abstract sites. */
  final case object AbstractUndefined extends AbstractLink

  /** A class representing free abstract sites. */
  final case object AbstractStub extends AbstractLink

  /** A class representing wildcard links at abstract sites. */
  final case class AbstractWildcard(
    agentState: Option[AbstractAgentState],
    siteState: Option[AbstractSiteState],
    linkState: Option[AbstractLinkState]) extends AbstractLink

  /** A class representing actual links at abstract sites. */
  final case class AbstractLinked(
    id: LinkId, state: AbstractLinkState) extends AbstractLink


  /** A class representing abstract sites. */
  final case class AbstractSite(state: AbstractSiteState, link: AbstractLink)
      extends PartialAbstractSite {

    @inline def toAbstractSite = this
  }

  /** A class representing abstract agents. */
  final case class AbstractAgent(
    state: AgentState, sites: Seq[(SiteState, AbstractLink)])
      extends PartialAbstractAgent {

    @inline def toAbstractAgent = this
    @inline override def toAbstractPattern: AbstractPattern =
      AbstractPattern(Vector(this))
  }

  /** An abstract class to build patterns. */
  final case class AbstractPattern(
    agents: Vector[AbstractAgent] = Vector(),
    siteGraphString: String = "") extends PartialAbstractPattern {

    @inline override def +:(that: AbstractAgent): AbstractPattern = {
      AbstractPattern(that +: agents, siteGraphString)
    }

    @inline override def :+(that: AbstractAgent): AbstractPattern = {
      AbstractPattern(agents :+ that, siteGraphString)
    }

    @inline override def toAbstractPattern: AbstractPattern = this

    override def toPattern: Pattern = {

      val linkMap = new mutable.HashMap[
        LinkId, List[(AgentIndex, SiteIndex, AbstractLinkState)]]() withDefaultValue Nil

      // Create agents
      val pb = new Pattern.Builder(siteGraphString)
      for ((u, i) <- agents.zipWithIndex) {
        val v = pb += u.state
        for (((sstate, link), j) <- u.sites.zipWithIndex) {
          val x = v += sstate
          link match {
            case AbstractUndefined         => { }
            case AbstractStub              => x define Pattern.Builder.Stub
            case AbstractWildcard(a, s, l) => {
              val aso = a map (_.toAgentState)
              val sso =
                for (as <- aso; ss <- s)
                yield ss.toSiteState(as.agentStateSet)
              val lso =
                for (ss <- sso; ls <- l)
                yield ls.toLinkState(sstate.siteStateSet, ss.siteStateSet)
              x define Pattern.Builder.Wildcard(aso, sso, lso)
            }
            case AbstractLinked(ln, ls)    =>
              linkMap += ((ln, ((i, j, ls) :: linkMap(ln))))
          }
        }
      }

      // Connect links
      for (l <- linkMap) l match {
        case (_, List((i1, j1, l1), (i2, j2, l2))) => {
          val s1 = pb.agents(i1).sites(j1)
          val s2 = pb.agents(i2).sites(j2)
          val t1 = s1.state.siteStateSet
          val t2 = s2.state.siteStateSet
          val ls1 = l1.toLinkState(t1, t2)
          val ls2 = l2.toLinkState(t2, t1)
          s1 connect (s2, ls1, ls2)
        }
        case (_, Nil) => {}
        case (l, List(_)) => throw new IllegalStateException(
          "dangling link with label " + l)
        case (l, _) => throw new IllegalStateException(
          "attempt to create hyperlink with label " + l)
      }

      // Build the pattern
      pb.build
    }
  }


  // -- Sugar for pattern and mixture construction. --

  /** Convert partial abstract patterns into patterns. */
  implicit def partialAbstractPatternToPattern(
    p: PartialAbstractPattern): Pattern = p.toPattern

  // FIXME: These should become redundant once the Parser.AST has been
  // replaced by Abstract* classes.
  def agentStateNameToAbstractAgentState(n: AgentStateName): AbstractAgentState
  def siteStateNameToAbstractSiteState(n: SiteStateName): AbstractSiteState
  def linkStateNameToAbstractLinkState(n: LinkStateName): AbstractLinkState

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
  implicit def stringToPattern(expr: String): Pattern = {

    val ast = parseSiteGraph(expr)

    // Add agents to builder
    var pb = new AbstractPattern(Vector(), expr)
    for (AST.Agent(state, intf) <- ast) {
      val astate = agentStateNameToAbstractAgentState(state)
      val sites = for (s <- intf) yield {
        val link = s.link match {
          case AST.Undefined         => AbstractUndefined
          case AST.Stub              => AbstractStub
          case AST.Wildcard(a, s, l) => AbstractWildcard(
            a map agentStateNameToAbstractAgentState,
            s map siteStateNameToAbstractSiteState,
            l map linkStateNameToAbstractLinkState)
          case AST.Linked(lsn)       => AbstractLinked(
            lsn.id, linkStateNameToAbstractLinkState(lsn))
        }
        new AbstractSite(
          siteStateNameToAbstractSiteState(s.state), link)
      }
      pb = pb :+ astate(sites: _*)
    }

    // Build
    pb.toPattern
  }

  /**
   * Build a KaSpace mixture from a string.
   *
   * This method first builds a [[Patterns#Pattern]] from a string and
   * subsequently converts it into a [[Mixtures#Mixture]].
   *
   * @param expr the string to build the mixture from.
   * @return a mixture corresponding to the expression `expr`.
   */
  implicit def stringToMixture(expr: String) = Mixture(stringToPattern(expr))

  /**
   * Convert a pair `(lhs, rhs)` of site graph expressions into an
   * action.
   */
  implicit def stringPairToKappaAction(lr: (String, String))(
    implicit ab: ActionBuilder): Action =
    ab(stringToPattern(lr._1), stringToPattern(lr._2))

  implicit def scToInterpolator(sc: StringContext): Interpolator =
    new Interpolator(sc)

  final class Interpolator(sc: StringContext) {
    def p(args: Any*): Pattern = stringToPattern( sc.s(args :_*) )
    def m(args: Any*): Mixture = stringToMixture( sc.s(args :_*) )
  }
}

