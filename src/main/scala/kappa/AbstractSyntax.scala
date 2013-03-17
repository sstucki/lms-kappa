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
      with Rules
      with Parser =>

  // -- Nodes of abstract syntax trees and their builders. --

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

    /** Append an abstract pattern to this partial abstract pattern.  */
    @inline def ++(that: AbstractPattern): AbstractPattern =
      this.toAbstractPattern ++ that

    /** Append an abstract action to this partial abstract pattern.  */
    @inline def ++(that: AbstractAction): AbstractAction =
      AbstractAction(this ++ that.lhs, that.rhs)

    /** Append an abstract rule tail to this partial abstract pattern.  */
    @inline def ++(that: AbstractRuleTail): AbstractRuleTail =
      that.prependAbstractPattern(this.toAbstractPattern)

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def +:(that: AbstractAgent): AbstractPattern =
      that +: this.toAbstractPattern

    /** Prepend an abstract action to this partial abstract pattern.  */
    @inline def +:(that: AbstractAction): AbstractAction =
      that ++ this.toAbstractPattern

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def ::(that: AbstractAgent): AbstractPattern =
      that +: this

    /** Prepend an abstract action to this partial abstract pattern.  */
    @inline def ::(that: AbstractAction): AbstractAction =
      that +: this

    /**
     * Build an abstract action from this partial abstract pattern and
     * an abstract agent.
     */
    @inline def ->(that: AbstractAgent): AbstractAction =
      AbstractAction(this.toAbstractPattern, that.toAbstractPattern)

    /** Build an action from this partial abstract pattern and a pattern. */
    @inline def ->(that: Pattern)(implicit ab: ActionBuilder): Action =
      ab(this.toPattern, that)

    /**
     * Build an action from this partial abstract pattern and the
     * empty pattern.
     */
    @inline def ->()(implicit ab: ActionBuilder): Action =
      ab(this.toPattern, AbstractPattern().toPattern)

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

    @inline def toAbstractSite: AbstractSite = this.!-

    /** Creates a site state from this abstract site state. */
    def toSiteState(agentStateSet: AgentStateSet): SiteState
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

  /**
   * A class representing abstract link states.
   *
   * NOTE: This class plays a dual role as an abstract link as well as
   * the state of that link.
   */
  abstract class AbstractLinkState extends AbstractLink {

    // FIXME: Still storing the source and target of a link in the
    // state itself, but that information should be stored in the
    // contact graph (ie as we do with Patterns).

    /** The link ID of this abstract link. */
    def id: LinkId

    /** Creates a link state from this abstract link state. */
    def toLinkState(source: SiteStateSet, target: SiteStateSet): LinkState
  }


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

  /** A class representing abstract patterns. */
  final case class AbstractPattern(
    agents: Vector[AbstractAgent] = Vector(),
    siteGraphString: String = "") extends PartialAbstractPattern {

    @inline override def :+(that: AbstractAgent): AbstractPattern = {
      AbstractPattern(agents :+ that, siteGraphString)
    }

    @inline override def ++(that: AbstractPattern): AbstractPattern =
      AbstractPattern(this.agents ++ that.agents, siteGraphString)

    @inline override def +:(that: AbstractAgent): AbstractPattern = {
      AbstractPattern(that +: agents, siteGraphString)
    }

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
        val v = pb += u.state
        for ((sstate, link) <- u.sites) {
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
            case l: AbstractLinkState    => {
              val li = l.id
              linkMap += ((li, (x, l) :: linkMap(li)))
            }
          }
        }
      }

      // Connect links
      for (l <- linkMap) l match {
        case (_, List((s1, l1), (s2, l2))) => {
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
      new AbstractRule(action, that.toRate(action.lhs))
    }

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def +:(that: AbstractAgent): AbstractAction =
      AbstractAction(that +: lhs, rhs)

    /** Prepend an abstract agent to this partial abstract pattern.  */
    @inline def ::(that: AbstractAgent): AbstractAction =
      that +: this

    /**
     * Build a rule that follows mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant
     */
    def :@(rate: => Double)(implicit ab: ActionBuilder): AbstractRule = {
      val action = toAction
      new AbstractRule(action, () => action.lhs.inMix * rate)
    }

    /**
     * Build a rule that follow an arbitrary rate law.
     *
     * @param law kinetic law expression
     */
    def !@(law: => Double)(implicit ab: ActionBuilder): AbstractRule =
      new AbstractRule(toAction, () => law)

    /** Convert this abstract action into an action. */
    @inline def toAction()(implicit ab: ActionBuilder): Action =
      ab(lhs.toPattern, rhs.toPattern)
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

  /** A class representing abstract Rules. */
  final case class AbstractRule(action: Action, rate: () => Double)
      extends PartialAbstractRule {
    def toRule: Rule = new Rule(action, rate)
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
          case AST.Linked(lsn)       => linkStateNameToAbstractLinkState(lsn)
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


  // -- Sugar for action and rule construction. --

  /** Convert abstract actions into actions. */
  implicit def abstractActionToAction(a: AbstractAction)(
    implicit ab: ActionBuilder): Action = a.toAction

  /** Convert abstract rules into rules. */
  implicit def abstractRuleToRule(r: AbstractRule): Rule = r.toRule

  /** Convert an abstract rule to a rule and register it. */
  def registerRule(r: AbstractRule) { registerRule(r.toRule) }

  /**
   * Convert a sequence of partial abstract rules into a sequence of
   * rules.
   */
  def partialAbstractRulesToRules(prs: Seq[PartialAbstractRule])(
    implicit ab: ActionBuilder): Seq[Rule] = {

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
    val rs = new mutable.ArrayBuffer[Rule]()
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
      case t: AbstractRuleTail       => pr match {
        case None                    => pr = Some(t)
        case Some(a: AbstractAction) => {
          rs += (a ++ t).toRule
          pr = None
        }
        case _ => failIncomplete(pr.get)
      }
      case r: AbstractRule => {
        if (!pr.isEmpty) failIncomplete(pr.get)
        else rs += r.toRule
      }
    }
    if (!pr.isEmpty) failIncomplete(pr.get)
    rs
  }
}

