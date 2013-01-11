package kappa

trait LanguageContext {
  type AgentState
  type SiteState
  type LinkState

  type AgentName    = String
  type AgentNameId  = Int
  type SiteName     = String
  type SiteId       = Int
  type AgentId      = Int
  type AgentStateId = Int
  type SiteStateId  = Int
  type LinkStateId  = Int
}

trait KappaContext {
  type AgentState = String
  type SiteState  = String
  type LinkState  = String
}

trait KaSpaceContext {
  type Radius = Double
  case class Position(x: Double, y: Double, z: Double)
  type Matrix = Vector[Vector[Double]] // dummy... at some later point we should use Scalala probably

  type AgentState = Radius
  type SiteState  = Position
  type LinkState  = Matrix
}

