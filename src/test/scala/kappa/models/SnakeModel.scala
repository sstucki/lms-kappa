package kappa.models

import org.scalatest.FlatSpec
import scala.math._

import kappa.{ KaSpaceModel, Orientation, Position }


class SnakeModel extends KaSpaceModel with FlatSpec {

  // Agent radius
  val radius = 1

  // Site positions
  val posL = Position(-2, 0, 0)
  val posR = Position( 2, 0, 0)

  // Link orientations
  val w180 = Orientation()
  val w90LR = Orientation.z(-Pi/2)
  val w90RL = Orientation.z( Pi/2)

  // Contact graph
  contactGraph = s"A:{$radius}(l:{$posL}!{1:{$w180,$w90LR}}, r:{$posR}!{1:{$w180,$w90RL}})"
  /*
  // Rules
  withRules(
    "A(r), A(l)" -> s"A(r!1:$w180) , A(l!1:$w180) , A:$radius(l:$posL, r:$posR)" :@ 1,  // r1
    "A(r), A(l)" -> s"A(r!1:$w90RL), A(l!1:$w90LR), A:$radius(l:$posL, r:$posR)" :@ 1)  // r2
  */
  // Observables
  withObs("straight links")(s"A(r!1:$w180), A(l!1:$w180)")
  withObs("90 deg turns")(s"A(r!1:$w90RL), A(l!1:$w90LR)")
  withObs("free A")(s"A(l:$posL, r:$posR)")

  // Mixture
  withInit(m"A:$radius(l:$posL, r:$posR)" * 2)

  // Simulate!
  maxEvents = 500
  //run()
}

