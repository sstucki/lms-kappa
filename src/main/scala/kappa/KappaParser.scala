package kappa

trait KappaParser extends KappaLikeParser {
  this: KappaContext =>

  type AgentLabel = Unit
  type  SiteLabel = String
  type  LinkLabel = Unit

  lazy val agentLabel: Parser[AgentLabel] = failure("agent states are not allowed in Kappa")
  lazy val  siteLabel: Parser[ SiteLabel] = """\w+""".r
  lazy val  linkLabel: Parser[ LinkLabel] = failure("link states are not allowed in Kappa")
}

