package kappa

import scala.collection.mutable

import scala.language.implicitConversions


/** A class representing Kappa models. */
class KappaModel(val contactGraph: String) extends Model with KappaContext
    with KappaActions with KappaParser with KappaSymbols {

  // -- Sugar for pattern construction. --

  /** A class to build Kappa sites. */
  final class KappaSiteBuilder(
    val state: SiteState, val link: KappaSiteBuilder.Link) {
    import KappaSiteBuilder._

    @inline def ? : KappaSiteBuilder =
      new KappaSiteBuilder(state, Undefined)
    @inline def !- : KappaSiteBuilder =
      new KappaSiteBuilder(state, Stub)
    @inline def !(li: Int): KappaSiteBuilder =
      new KappaSiteBuilder(state, Linked(li))
    @inline def !(wc: Pattern.Wildcard): KappaSiteBuilder =
      new KappaSiteBuilder(
        state, Wildcard(wc.agentState, wc.siteState, wc.linkState))
    @inline def !* : KappaSiteBuilder =
      new KappaSiteBuilder(state, Wildcard(None, None, None))

    @inline def toSite = {
      val link = this.link match {
        case KappaSiteBuilder.Undefined         => Pattern.Undefined
        case KappaSiteBuilder.Stub              => Pattern.Stub
        case KappaSiteBuilder.Wildcard(a, s, l) => Pattern.Wildcard(a, s, l)
        case KappaSiteBuilder.Linked(_)         => Pattern.Undefined
      }
      Pattern.Site(this.state, link)
    }
  }

  /** Companion object of the Kappa site builder. */
  object KappaSiteBuilder {

    sealed abstract class Link
    final case object Undefined extends Link
    final case object Stub extends Link
    final case class Wildcard(
      agentState: Option[AgentState],
      siteState: Option[SiteState],
      linkState: Option[LinkState]) extends Link
    final case class Linked(to: Int) extends Link

    val defaultLinkState = new LinkState(Some(0))
    linkStateNames     = linkStateNames    :+ "."
    linkStateNameSyms  = linkStateNameSyms + ("." -> 0)
  }

  /** A class to build Kappa agents. */
  final class KappaAgentBuilder(val state: AgentState) {
    @inline def apply(siteBuilders: KappaSiteBuilder*): KappaAgentWrapper =
      new KappaAgentWrapper(state, siteBuilders)
  }

  /** A class to wrap Kappa agents. */
  final class KappaAgentWrapper(
    val state: AgentState, val siteBuilders: Seq[KappaSiteBuilder]) {
    @inline def ~(that: KappaAgentWrapper): KappaPatternBuilder =
      this.toBuilder ~ that
    @inline def toBuilder: KappaPatternBuilder =
      new KappaPatternBuilder(
        new mutable.ArrayBuffer[KappaAgentWrapper]() += this)
    @inline def toPattern: Pattern = this.toBuilder.toPattern
  }

  /** A class to build Kappa patterns. */
  final class KappaPatternBuilder(
    val agentBuilders: mutable.Buffer[KappaAgentWrapper]) {
    @inline def ~(that: KappaAgentWrapper): KappaPatternBuilder =
      new KappaPatternBuilder(agentBuilders += that)
    @inline def ->(that: KappaPatternBuilder): Action =
      KappaAction(this.toPattern, that.toPattern)
    def toPattern: Pattern = {

      val linkMap = new mutable.HashMap[
        Int, (AgentIndex, SiteIndex)]()
      val links = new mutable.ArrayBuffer[
        (AgentIndex, SiteIndex, AgentIndex, SiteIndex)]()

      // Create agents
      var p = Pattern()
      for ((u, i) <- agentBuilders.zipWithIndex) {
        val sites = for ((sb, j) <- u.siteBuilders.zipWithIndex) yield {
          sb.link match {
            case KappaSiteBuilder.Linked(li) => {
              linkMap.get(li) match {
                case Some((-1, _)) => throw new IllegalStateException(
                  "attempt to create hyperlink")
                case Some((i2, j2)) => {
                  links += ((i, j, i2, j2))
                  linkMap += ((li, (-1, 0)))
                }
                case None => {
                  linkMap += ((li, (i, j)))
                }
              }
            }
            case _ => { }
          }
          sb.toSite
        }
        p = p :+ Pattern.Agent(u.state, sites.toArray)
      }

      // Connect links
      for ((i1, j1, i2, j2) <- links) {
        p = p connect (
          i1, j1, KappaSiteBuilder.defaultLinkState,
          i2, j2, KappaSiteBuilder.defaultLinkState)
      }
      p
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
      case Pattern.Wildcard(a, s, l) => KappaSiteBuilder.Wildcard(a, s, l)
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

  /*
  initSymbols(parseContactGraph(contactGraph) match {
    case Success(cg, _) => cg
    case msg => throw new IllegalArgumentException(
      "given contact graph is invalid: " + msg)
  })
  */
}
