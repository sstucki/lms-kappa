package kappa

trait KappaParser extends Parser {
  this: KappaContext =>

  lazy val agentType : Parser[AgentType] = ident
  lazy val siteName  : Parser[SiteName]  = ident

  lazy val agentState : Parser[AgentStateName] = failure("agent states are not allowed in Kappa")
  lazy val siteState  : Parser[SiteStateName]  = """\w+""".r
  lazy val linkState  : Parser[LinkStateName]  = failure("link states are not allowed in Kappa")
}

