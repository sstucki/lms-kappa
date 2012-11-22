package kappa

import collection.TraversableLike


trait SiteGraph[A, S, L] extends TraversableLike[A, SiteGraph[A, S, L]] {

  // Agents
  trait Agent {
    def agentType: A
    def siteState(i: Int): S
    def siteLink(i: Int): Link
    def connectSite(from: Int, to: Site)

    // FIXME: forcing A, S and L of G2 to be the same as for this
    // graph seems unnecessarily restricitive.  As long as the types
    // allow for a meaningful comparison (in the sense of a partial
    // order) they should be allows.  Maybe a view bound would do a
    // better job here?
    def <=[G2 <: SiteGraph[A, S, L]](b: G2#Agent): Boolean
  }

  // Sites
  trait Site {
    def from: A
    def index: Int
    def state: S
  }

  // Links
  sealed abstract class Link;
  case object Undefined extends Link
  case object Stub extends Link
  case class Linked(from: Site, to: Site, state: L) extends Link
  case class Dangling(from: Site, state: L) extends Link

  def connectSites(from: Site, to: Site, state: L)
  def connectSites(l: Linked)
  def disconnectSites(from: Site, to: Site)
  def disconnectSites(l: Linked)

  // TODO: add embeddings...
  //def extendEmbedding[G1 <: SiteGraph, G2 <: SiteGraph](
  //  e: Embedding[G1, G2]): Embedding[, G]
}
