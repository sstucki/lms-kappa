package kappa.immutable

import kappa._
import scala.collection.immutable.{Vector, VectorBuilder}

class SiteGraph[A, S, L] private (
  private val agentStates: Vector[A],
  private val siteStates: Vector[Vector[S]],
  private val siteLinks: Vector[Vector[Link[Int, Int]]])
extends kappa.SiteGraph[A, S, L] {

  type Agent = Int
  type SiteName = Int

  // Convenience aliases
  private type SG = SiteGraph[A, S, L]
  private type SGSite = Site[Agent, SiteName]
  private type SGLink = Link[Agent, SiteName]

  // Site graph ops
  /* FIXME: Implement */
  //def ++(that: kappa.SiteGraph[A, S, L]): SG

  def ++(that: SG) = {
    val nas = that.agentStates.size
    val ls = that.siteLinks map (_ map {
      case Linked(Site(a, n), s) => Linked(Site(a + nas, n), s)
      case l => l 
    })
    new SG(this.agentStates ++ that.agentStates,
           this.siteStates ++ that.siteStates,
           this.siteLinks ++ ls)
  }

  @inline
  private def mkVector[A](a: A, size: Int): Vector[A] = {
    val bldr = new VectorBuilder[A]();
    bldr.sizeHint(size)
    var i: Int = 0;
    while (i < size) { bldr += a; i += 1 }
    bldr.result()
  }

  // Agent ops

  def iterator = (0 until agentStates.size).iterator

  def agentState(agent: Agent) = this.agentState(agent)

  def +(as: A, sts: Vector[S]) = {
    val ls = mkVector(Undefined, sts.size)
    new SiteGraph(agentStates :+ as, siteStates :+ sts, siteLinks :+ ls)
  }

  // Site state ops

  def siteIterator(agent: Agent) =
    (for (i <- 0 until siteStates(agent).size) yield Site(agent, i)).iterator

  def siteState(from: SGSite) =
    siteStates(from.agent)(from.name)

  def updatedSiteState(from: SGSite, state: S): SG =
    new SG(
      agentStates, siteStates updated (
        from.agent, siteStates(from.agent) updated (from.name, state)),
      siteLinks)

  // Links ops

  @inline
  private def updatedSiteLinks(ls: Vector[Vector[SGLink]],
                               from: SGSite, l: SGLink) =
    ls updated (from.agent, ls(from.agent) updated (from.name, l))

  // Link ops
  def siteLink(from: SGSite) = siteLinks(from.agent)(from.name)

  def updatedLinkState(from: SGSite, state: L) = {
    val l = siteLink(from) match {
      case Undefined => throw new IllegalArgumentException(
        "Attempt to update state of undefined link")
      case Stub => throw new IllegalArgumentException(
        "Attempt to update state of stub")
      case Linked(to, _) => Linked(to, state)
      case Wildcard(_) => Wildcard(state)
    }
    new SG(agentStates, siteStates, updatedSiteLinks(siteLinks, from, l))
  }

  def connect(from: SGSite, to: SGSite, state: L) = {
    val ls = siteLink(from) match {
      case Undefined => throw new IllegalArgumentException(
        "Attempt to connect undefined link at " + from)
      case l @ Stub => siteLink(to) match {
        case Undefined => throw new IllegalArgumentException(
        "Attempt to connect undefined link at " + to)
        case l @ Stub => updatedSiteLinks(
          updatedSiteLinks(siteLinks, from, Linked(to, state)),
          to, Linked(from, state))
        case Linked(_, _) | Wildcard(_) => throw new IllegalArgumentException(
          "Attempt to connect already connected link at " + to)
      }
      case Linked(_, _) | Wildcard(_) => throw new IllegalArgumentException(
        "Attempt to connect already connected link at " + from)
    }
    new SG(agentStates, siteStates, ls)
  }

  def disconnect(from: SGSite) = {
    val ls = siteLink(from) match {
      case Undefined => throw new IllegalArgumentException(
        "Attempt to disconnect undefined link at " + from)
      case Stub => throw new IllegalArgumentException(
        "Attempt to disconnect already disconnected link at " + from)
      case Linked(to, _) => siteLink(to) match {
        case Undefined => throw new IllegalArgumentException(
          "Attempt to disconnect undefined link at " + to)
        case Stub | Wildcard(_) => throw new IllegalArgumentException(
          "Inconsistent link relation: (" + from + ", " + siteLink(from) +
          "), (" + to + ", " + siteLink(to) + ")")
        case Linked(from2, _) if from != from2 =>
          throw new IllegalArgumentException(
            "Inconsistent link relation: (" + from + ", " + siteLink(from) +
            "), (" + to + ", " + siteLink(to) + ")")
        case Linked(_, _) => updatedSiteLinks(
          updatedSiteLinks(siteLinks, from, Stub), to, Stub)
      }
      case Wildcard(_) => updatedSiteLinks(siteLinks, from, Stub)
    }
    new SG(agentStates, siteStates, ls)
  }

  def define(from: SGSite) = {
    val ls = siteLink(from) match {
      case Undefined => updatedSiteLinks(siteLinks, from, Stub)
      case _ => throw new IllegalArgumentException(
        "Attempt to define already defined link at " + from)
    }
    new SG(agentStates, siteStates, ls)
  }

  def undefine(from: SGSite) = {
    val ls = siteLink(from) match {
      case Undefined => throw new IllegalArgumentException(
        "Attempt to undefine already undefined link at " + from)
      case Stub => updatedSiteLinks(siteLinks, from, Undefined)
      case Linked(_, _) => throw new IllegalArgumentException(
        "Attempt to undefine link at " + from)
      case Wildcard(_) => throw new IllegalArgumentException(
        "Attempt to undefine wildcard at " + from)
    }
    new SG(agentStates, siteStates, ls)
  }
}
