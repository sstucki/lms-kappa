package kappa

import org.scalatest.FlatSpec
import scala.math._

class StackedTriangle extends KaSpaceModel with FlatSpec {

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
  withRules(
    "A(r), A(l)" -> s"A(r!1:$wRL), A(l!1:$wLR)" :@ 1,  // bind l-r
    "A(r!1), A(l!1)" -> "A(r), A(l)" :@ 1,             // unbind l-r
    "A(n), A(n)" -> s"A(n!1:$wNN), A(n!1:$wNN)" :@ 1,  // bind n-n
    "A(n!1), A(n!1)" -> "A(n), A(n)" :@ 1)             // unbind n-n

  // Mixture
  withInit(m"A:$radius (l:$posL, r:$posR, n:$posN)" * 50)

  // Observables
  withObs("triangle")("A(l!1, r!2), A(l!2, r!3), A(l!3, r!1)")
  // Dimer
  withObs("AA")("A(r!1), A(l!1)")
  // Trimer
  withObs("AAA")("A(r!1), A(l!1, r!2), A(l!2)")
  // Tetramer
  withObs("AAAA")("A(r!1), A(l!1, r!2), A(l!2, r!3), A(l!3)")

  // Simulate!
  withMaxEvents(100)
  run
}

