package kappa

import org.scalatest.FlatSpec
import scala.math._

class StackedTriangle extends KaSpaceModel with FlatSpec
{
  // Agent radius
  val radius = 1

  // Site positions
  val posL = Position.fromSpherical(2, Pi * 2 / 3, 0)
  val posR = Position.fromSpherical(2, Pi / 3, 0)
  val posN = Position.fromSpherical(2, 0, Pi / 2)

  // Link orientations
  val wLR = Orientation.z(-Pi * 2 / 3)
  val wRL = Orientation.z( Pi * 2 / 3)

  val wNN = Orientation.y(Pi)

  // Contact graph
  contactGraph = s"""A:{$radius} (l:{$posL}!{1:{$wLR}},
                                  r:{$posR}!{1:{$wRL}},
                                  n:{$posN}!{2:{$wNN},2:{$wNN}})"""

  // Rules
  val bindRL = "A(r), A(l)" -> s"A(r!1:$wRL), A(l!1:$wLR)" :@ 1
  val unbindLR = "A(r!1), A(l!1)" -> "A(r), A(l)" :@ 1
  val bindNN = "A(n), A(n)" -> s"A(n!1:$wNN), A(n!1:$wNN)" :@ 1
  val unbindNN = "A(n!1), A(n!1)" -> "A(n), A(n)" :@ 1

  // Mixture
  withInit(m"A:$radius (l:$posL, r:$posR, n:$posN)" * 50)

  // Observables
  withObs("A(l!1, r!2), A(l!2, r!3), A(l!3, r!1)", "triangle")
  // Dimer
  withObs("A(r!1), A(l!1)", "AA")
  // Trimer
  withObs("A(r!1), A(l!1, r!2), A(l!2)", "AAA")
  // Tetramer
  withObs("A(r!1), A(l!1, r!2), A(l!2, r!3), A(l!3)", "AAAA")

  // Simulate!
  withMaxEvents(100)
  run
}

