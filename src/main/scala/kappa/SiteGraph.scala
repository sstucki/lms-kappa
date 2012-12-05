package kappa

trait SiteGraph[A, S, L] {
  self =>

  type Agent
  type SiteName

  // Convenience aliases
  private type SG = SiteGraph[A, S, L] {
    type Agent = self.Agent
    type SiteName = self.SiteName
  }
  private type SGSite = Site[Agent, SiteName]
  private type SGLink = Link[Agent, SiteName]

  // Site graph ops
  // TODO:
  //def ++(that: SiteGraph[A, S, L]): SG
  def components: Seq[SG]

  // Agent ops
  def iterator: Iterator[Agent]
  def agentState(agent: Agent): A
  def +(as: A, sts: Vector[S]): SG

  // Site state ops
  def siteIterator(agent: Agent): Iterator[SGSite]
  def siteState(from: SGSite): S
  def updatedSiteState(from: SGSite, state: S): SG

  // Link ops
  def siteLink(from: SGSite): SGLink
  def updatedLinkState(from: SGSite, state: L): SG
  def connect(from: SGSite, to: SGSite, state: L): SG
  def disconnect(from: SGSite): SG
  def define(from: SGSite): SG
  def undefine(from: SGSite): SG
  def +(from: SGSite, to: SGSite, state: L): SG =
    this define from define to connect (from, to, state)
  def -(from: SGSite, to: SGSite): SG =
    this disconnect from undefine from undefine to
}

// Sites
case class Site[AN, SN](agent: AN, name: SN)

// Links
sealed abstract class Link[+AN, +SN];
case object Undefined extends Link[Nothing, Nothing]
case object Stub extends Link[Nothing, Nothing]
case class Linked[AN, SN, L](to: Site[AN, SN], state: L) extends Link[AN, SN]
case class Wildcard[L](state: L) extends Link[Nothing, Nothing]

