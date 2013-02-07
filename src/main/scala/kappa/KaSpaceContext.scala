package kappa

// trait KaSpaceContext extends KappaLikeContext {

//   // TODO: If we use an environment (aka symbol table), then these
//   // should be references to symbols (i.e. the AgentStateId?).  If
//   // e.g. Site states are optional, it should be an option of symbol
//   // reference, etc.

//   // TODO: Again, if we use a symbol table, then these should be
//   // references to symbols, probably including the geometry!  Are
//   // there any advantages to keeping geometry as non-symbols?  I.e. in
//   // the KaSpace paper, geometry is defined to be restricted to a
//   // finite subset of radii, positions and matrices, which suggests
//   // that we might allocate symbols for them.  Is there any reason why
//   // this should not be so?
//   type Radius = Double
//   case class Position(x: Double, y: Double, z: Double)
//   type Matrix = Vector[Vector[Double]] // dummy... at some later point we should use Scalala probably

//   // FIXME: should these be proper classes?
//   type AgentState = Pair[String, Option[Radius]]
//   type SiteState  = Pair[Option[String], Option[Position]]
//   type LinkState  = Pair[Option[String], Option[Matrix]]

//   val radiusPo = partialOrderingFromEquiv(Ordering.Double)
//   val posPo = partialOrderingFromEquiv(Equiv.universal[Position])
//   val orientationPo = partialOrderingFromEquiv(Equiv.universal[Matrix])

//   val apo = productPartialOrdering(KappaContext.apo, radiusPo)
//   val spo = productPartialOrdering(KappaContext.spo, posPo)
//   val lpo = productPartialOrdering(KappaContext.lpo, orientionPo)
// }

