package kappa

import scala.collection.mutable

import scala.language.implicitConversions


/** A class representing Kappa models. */
class KappaModel extends Model with KappaContext
    with KappaActions with KappaParser with KappaSymbols {

  // -- Sugar for pattern construction. --

  /** A class to build Kappa sites. */
  final class KappaSiteBuilder(
    val state: KappaSiteState, val link: KappaSiteBuilder.Link) {
    import KappaSiteBuilder._

    @inline def ? : KappaSiteBuilder =
      new KappaSiteBuilder(state, Undefined)
    @inline def !- : KappaSiteBuilder =
      new KappaSiteBuilder(state, Stub)
    @inline def !(li: Int): KappaSiteBuilder =
      new KappaSiteBuilder(state, Linked(li))
    @inline def !(wc: Pattern.Wildcard): KappaSiteBuilder =
      new KappaSiteBuilder(
        state, Wildcard(wc.agentState map (_.atype), wc.siteState))
    @inline def !* : KappaSiteBuilder =
      new KappaSiteBuilder(state, Wildcard(None, None))
  }

  /** Companion object of the Kappa site builder. */
  object KappaSiteBuilder {

    type BondLabel = Int

    sealed abstract class Link
    final case object Undefined extends Link
    final case object Stub extends Link
    final case class Wildcard(
      agentState: Option[AgentType],
      siteState: Option[KappaSiteState]) extends Link
    final case class Linked(to: BondLabel) extends Link
  }

  /** A class to build Kappa agents. */
  final class KappaAgentBuilder(val state: AgentState) {

    @inline def apply(siteBuilders: KappaSiteBuilder*): KappaAgentWrapper = {

      def undefinedSite(name: SiteName) = new KappaSiteBuilder(
        KappaSiteState(state.atype, name, None), KappaSiteBuilder.Undefined)

      // First complete the interface by filling in undefined sites
      val intfMap = (for (sb <- siteBuilders) yield (sb.state.name, sb)).toMap
      val completeIntf =
        siteNames(state.atype) map (intfMap withDefault undefinedSite)

      // Then create the agent wrapper
      new KappaAgentWrapper(state, completeIntf)
    }
  }

  /** A class to wrap Kappa agents. */
  final class KappaAgentWrapper(
    val state: AgentState, val siteBuilders: Seq[KappaSiteBuilder]) {

    @inline def ~(that: KappaAgentWrapper): KappaPatternBuilder =
      this.toBuilder ~ that

    @inline def toBuilder: KappaPatternBuilder =
      new KappaPatternBuilder() ~ this

    @inline def toPattern: Pattern = this.toBuilder.toPattern
  }

  /** A class to build Kappa patterns. */
  final class KappaPatternBuilder(
    val agentBuilders: Vector[KappaAgentWrapper] = Vector(),
    val siteGraphString: String = "") {

    @inline def ~(that: KappaAgentWrapper): KappaPatternBuilder = {
      new KappaPatternBuilder(agentBuilders :+ that, siteGraphString)
    }

    @inline def ->(that: KappaPatternBuilder): Action =
      KappaAction(this.toPattern, that.toPattern)

    def toPattern: Pattern = {

      import KappaSiteBuilder._

      val linkMap = new mutable.HashMap[
        BondLabel, List[(AgentIndex, SiteIndex)]]() withDefault Nil

      // Create agents
      val pb = new Pattern.Builder("")
      for ((u, i) <- agentBuilders.zipWithIndex) {
        val v = pb += u.state
        for ((sb, j) <- u.siteBuilders.zipWithIndex) {
          val x = v += sb.state
          sb.link match {
            case Stub           => x define Pattern.Builder.Stub
            case Wildcard(a, s) => x define Pattern.Builder.Wildcard(
              a map (KappaAgentState(_)), s, None)
            case Linked(li)     => linkMap += ((li, (i, j) :: linkMap(li)))
            case _              => { }
          }
        }
      }

      // Connect links
      for (l <- linkMap) l match {
        case (_, List((i1, j1), (i2, j2))) => {
          val s1 = pb.agents(i1).sites(j1)
          val s2 = pb.agents(i2).sites(j2)
          s1 connect (s2, KappaLinkState, KappaLinkState)
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
  implicit def siteStateToBuilder(t: SiteState): KappaSiteBuilder =
    new KappaSiteBuilder(t, KappaSiteBuilder.Stub)

  /** Convert sites into site builders. */
  implicit def siteToBuilder(s: Pattern.Site): KappaSiteBuilder = {
    val link = s.link match {
      case Pattern.Undefined         => KappaSiteBuilder.Undefined
      case Pattern.Stub              => KappaSiteBuilder.Stub
      case Pattern.Wildcard(a, s, _) =>
        KappaSiteBuilder.Wildcard(a map (_.atype), s)
      case Pattern.Linked(_, _, _)   => throw new IllegalArgumentException(
        "attempt to build pre-connected site")
    }
    new KappaSiteBuilder(s.state, link)
  }

  /** Convert agent states into agent builders. */
  implicit def agentStateToBuilder(t: AgentState): KappaAgentBuilder =
    new KappaAgentBuilder(t)

  /** Convert agent wrappers to patterns. */
  implicit def wrapperToPattern(w: KappaAgentWrapper): Pattern =
    w.toPattern

  /** Convert pattern builders to patterns. */
  implicit def builderToPattern(b: KappaPatternBuilder): Pattern =
    b.toPattern

  /** Convert pairs of pattern builders to actions. */
  implicit def builderPairToAction(
    lr: (KappaPatternBuilder, KappaPatternBuilder)): Action =
    KappaAction(lr._1.toPattern, lr._1.toPattern)


  /**
   * Build a Kappa pattern from a string.
   *
   * This method invokes the [[Parser]] to parse a Kappa expression.
   * It then walks the [[Parser.AST]] and builds a
   * [[Patterns.Pattern]] from the expression.
   *
   * @return a pattern corresponding to the expression `expr`.
   */
  implicit def stringToPattern(expr: String): Pattern = {

    import KappaSiteBuilder._

    val ast = parseSiteGraph(expr)

    var pb = new KappaPatternBuilder(Vector(), expr)
    for (AST.Agent(atype, astate, intf) <- ast) {
      val sites = for (s <- intf) yield {
        val link = s.lnk match {
          case AST.Undefined   => Undefined
          case AST.Stub        => Stub
          case AST.Wildcard    => Wildcard(None, None)
          case AST.Linked(lbl) => Linked(lbl)
        }
        new KappaSiteBuilder(KappaSiteState(atype, s.name, s.int), link)
      }
      pb = pb ~ (new KappaAgentBuilder(KappaAgentState(atype)))(sites: _*)
    }
    pb.toPattern
  }

  /*
  initSymbols(parseContactGraph(contactGraph) match {
    case Success(cg, _) => cg
    case msg => throw new IllegalArgumentException(
      "given contact graph is invalid: " + msg)
  })
  */
}
