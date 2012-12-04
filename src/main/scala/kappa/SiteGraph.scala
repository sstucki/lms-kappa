package kappa

// TODO: Should A, S, L really all be invariant?
trait SiteGraph[A, S, L] {

  type Agent
  type SiteName

  // Site graph ops
  // FIXME
  //def ++(that: SiteGraph[A, S, L]): SiteGraph[A, S, L]

  // Agent ops
  def iterator: Iterator[Agent]
  def agentState(agent: Agent): A
  def +(as: A, sts: Vector[S]): SiteGraph[A, S, L]

  // Site state ops
  def siteIterator(agent: Agent): Iterator[Site[Agent, SiteName]]
  def siteState(from: Site[Agent, SiteName]): S
  def updatedSiteState(from: Site[Agent, SiteName],
                       state: S): SiteGraph[A, S, L]

  // Link ops
  def siteLink(from: Site[Agent, SiteName]): Link[Agent, SiteName]
  //def +(kv: (Site, Link)): SiteGraph[A, S, L]
  //def -(from: Site): SiteGraph[A, S, L]
  def updatedLinkState(from: Site[Agent, SiteName],
                       state: L): SiteGraph[A, S, L]
  def connect(from: Site[Agent, SiteName], to: Site[Agent, SiteName],
              state: L): SiteGraph[A, S, L]
  def disconnect(from: Site[Agent, SiteName]): SiteGraph[A, S, L]
  def define(from: Site[Agent, SiteName]): SiteGraph[A, S, L]
  def undefine(from: Site[Agent, SiteName]): SiteGraph[A, S, L]
}

// Sites
case class Site[@specialized(Int) AN, @specialized(Int) SN](
  agent: AN, name: SN)

// Links
sealed trait Link[+AN, +SN];
case object Undefined extends Link[Nothing, Nothing]
case object Stub extends Link[Nothing, Nothing]
case class Linked[AN, SN, L](to: Site[AN, SN], state: L) extends Link[AN, SN]
case class Wildcard[L](state: L) extends Link[Nothing, Nothing]

