package kappa

import scala.collection.mutable

import scala.language.implicitConversions

class KaSpaceModel extends Model with KaSpaceContext with KaSpaceActions
    with KaSpaceParser with KaSpaceSymbols {

  /** Run a simulation of this model. */
  override def run() {

    // Run geometric soundness check to initialize
    // position/orientation of all agents in the mixture.
    print("# == Initialize mixture geometry...")
    if(!KaSpaceAction.checkGeometricSoundness(mix)) {
      println(" failed!")
      println("# == Geometrically unsound initial mixture. Terminating...")
    } else {
      println(" done.")

      // Run the standard simulator.
      super.run
    }
  }


  // -- Sugar for pattern construction. --

  /** A class to build KaSpace sites. */
  final class KaSpaceSiteBuilder(
    val state: KaSpaceSiteState, val link: KaSpaceSiteBuilder.Link) {
    import KaSpaceSiteBuilder._

    @inline def ? : KaSpaceSiteBuilder =
      new KaSpaceSiteBuilder(state, Undefined)
    @inline def !- : KaSpaceSiteBuilder =
      new KaSpaceSiteBuilder(state, Stub)
    @inline def !(li: Int, ls: Option[LinkStateName]): KaSpaceSiteBuilder =
      new KaSpaceSiteBuilder(state, Linked(li, ls))
    @inline def !(wc: Agent.Wildcard): KaSpaceSiteBuilder =
      new KaSpaceSiteBuilder(
        state, Wildcard(wc.agentState, wc.siteState, wc.linkState))
    @inline def !* : KaSpaceSiteBuilder =
      new KaSpaceSiteBuilder(state, Wildcard(None, None, None))
  }

  /** Companion object of the KaSpace site builder. */
  object KaSpaceSiteBuilder {

    type LinkLabel = Int

    sealed abstract class Link
    final case object Undefined extends Link
    final case object Stub extends Link
    final case class Wildcard(
      agentState: Option[KaSpaceAgentState],
      siteState: Option[KaSpaceSiteState],
      linkState: Option[KaSpaceLinkState]) extends Link
    final case class Linked(to: LinkLabel, state: Option[LinkStateName]) extends Link
  }

  /** A class to build KaSpace agents. */
  final class KaSpaceAgentBuilder(val state: AgentState) {

    @inline def apply(siteBuilders: KaSpaceSiteBuilder*): KaSpaceAgentWrapper = {

      def undefinedSite(name: SiteName) = new KaSpaceSiteBuilder(
        KaSpaceSiteState(state.atype, name, None), KaSpaceSiteBuilder.Undefined)

      // First complete the interface by filling in undefined sites
      val intfMap = (for (sb <- siteBuilders) yield (sb.state.name, sb)).toMap
      val completeIntf =
        siteNames(state.atype) map (intfMap withDefault undefinedSite)

      // Then create the agent wrapper
      new KaSpaceAgentWrapper(state, completeIntf)
    }
  }

  /** A class to wrap KaSpace agents. */
  final class KaSpaceAgentWrapper(
    val state: AgentState, val siteBuilders: Seq[KaSpaceSiteBuilder]) {

    @inline def ~(that: KaSpaceAgentWrapper): KaSpacePatternBuilder =
      this.toBuilder ~ that

    @inline def toBuilder: KaSpacePatternBuilder =
      new KaSpacePatternBuilder() ~ this

    @inline def toPattern: Pattern = this.toBuilder.toPattern
  }

  /** A class to build KaSpace patterns. */
  final class KaSpacePatternBuilder(
    val agentBuilders: Vector[KaSpaceAgentWrapper] = Vector(),
    val siteGraphString: String = "") {

    @inline def ~(that: KaSpaceAgentWrapper): KaSpacePatternBuilder = {
      new KaSpacePatternBuilder(agentBuilders :+ that, siteGraphString)
    }

    @inline def ->(that: KaSpacePatternBuilder): Action =
      KaSpaceAction(this.toPattern, that.toPattern)

    def toPattern: Pattern = {

      import KaSpaceSiteBuilder._

      val linkMap = new mutable.HashMap[
        LinkLabel, List[(AgentIndex, SiteIndex, Option[LinkStateName])]]() withDefaultValue Nil

      // Create agents
      val pb = new Pattern.Builder("")
      for ((u, i) <- agentBuilders.zipWithIndex) {
        val v = pb += u.state
        for ((sb, j) <- u.siteBuilders.zipWithIndex) {
          val x = v += sb.state
          sb.link match {
            case Stub              => x define Pattern.Builder.Stub
            case Wildcard(a, s, l) =>
              x define Pattern.Builder.Wildcard(a, s, l)
            case Linked(li, ls)    =>
              linkMap += ((li, (i, j, ls) :: linkMap(li)))
            case _                 => { }
          }
        }
      }

      // Connect links
      for (l <- linkMap) l match {
        case (_, List((i1, j1, l1), (i2, j2, l2))) => {
          val s1 = pb.agents(i1).sites(j1)
          val s2 = pb.agents(i2).sites(j2)
          val atype1 = s1.state.atype
          val sname1 = s1.state.name
          val atype2 = s2.state.atype
          val sname2 = s2.state.name
          val link1 = (atype1, sname1, atype2, sname2)
          val link2 = (atype2, sname2, atype1, sname1)
          val ls1 = KaSpaceLinkState(link1, l1)
          val ls2 = KaSpaceLinkState(link2, l2)
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

  /** Convert site states into site builders. */
  implicit def siteStateToBuilder(t: SiteState): KaSpaceSiteBuilder =
    new KaSpaceSiteBuilder(t, KaSpaceSiteBuilder.Stub)

  /** Convert sites into site builders. */
  implicit def siteToBuilder(
    s: Pattern.Agent#Site): KaSpaceSiteBuilder = {
    val link = s.link match {
      case Agent.Undefined         => KaSpaceSiteBuilder.Undefined
      case Agent.Stub              => KaSpaceSiteBuilder.Stub
      case Agent.Wildcard(a, s, l) => KaSpaceSiteBuilder.Wildcard(a, s, l)
      case Agent.Linked(_, _, _)   => throw new IllegalArgumentException(
        "attempt to build pre-connected site")
    }
    new KaSpaceSiteBuilder(s.state, link)
  }

  /** Convert agent states into agent builders. */
  implicit def agentStateToBuilder(t: AgentState): KaSpaceAgentBuilder =
    new KaSpaceAgentBuilder(t)

  /** Convert agent wrappers to patterns. */
  implicit def wrapperToPattern(w: KaSpaceAgentWrapper): Pattern =
    w.toPattern

  /** Convert pattern builders to patterns. */
  implicit def builderToPattern(b: KaSpacePatternBuilder): Pattern =
    b.toPattern

  /** Convert pairs of pattern builders to actions. */
  implicit def builderPairToAction(
    lr: (KaSpacePatternBuilder, KaSpacePatternBuilder)): Action =
    KaSpaceAction(lr._1.toPattern, lr._1.toPattern)


  /**
   * Build a KaSpace pattern from a string.
   *
   * This method invokes the [[Parser]] to parse a KaSpace expression.
   * It then walks the [[Parser.AST]] and builds a
   * [[Patterns.Pattern]] from the expression.
   *
   * @param expr the string to build the pattern from.
   * @return a pattern corresponding to the expression `expr`.
   */
  implicit def stringToPattern(expr: String): Pattern = {

    import KaSpaceSiteBuilder._

    val ast = parseSiteGraph(expr)

    // Collect link orientations
    val lstates =
      for (AST.LinkAnnot(lbl, state) <- ast) yield (lbl, state)
    val lstateMap = mutable.HashMap(lstates: _*)
    def getLinkState(lbl: LinkLabel): Option[LinkStateName] = {
      lstateMap.get(lbl) map { p => lstateMap += ((lbl, p.swap)); p._1 }
    }

    // Add agents to builder
    var pb = new KaSpacePatternBuilder(Vector(), expr)
    for (AST.Agent(atype, astate, intf) <- ast) {
      val sites = for (s <- intf) yield {
        val link = s.lnk match {
          case AST.Undefined   => Undefined
          case AST.Stub        => Stub
          case AST.Wildcard    => Wildcard(None, None, None)
          case AST.Linked(lbl) => Linked(lbl, getLinkState(lbl))
        }
        new KaSpaceSiteBuilder(KaSpaceSiteState(atype, s.name, s.int), link)
      }
      val ab = new KaSpaceAgentBuilder(KaSpaceAgentState(atype, astate))
      pb = pb ~ ab(sites: _*)
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
   * Convert a pair `(lhs, rhs)` of KaSpace pattern strings into a
   * KaSpace action.
   */
  implicit def stringPairToKappaAction(lr: (String, String)): Action =
    KaSpaceAction(stringToPattern(lr._1), stringToPattern(lr._2))

  implicit def scToKaSpace(sc: StringContext): Interpolator = new Interpolator(sc)

  class Interpolator(sc: StringContext) {
    def p(args: Any*): Pattern = stringToPattern( sc.s(args :_*) )
    def m(args: Any*): Mixture = stringToMixture( sc.s(args :_*) )
  }
}

