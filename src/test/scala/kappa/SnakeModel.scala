package kappa

import org.scalatest.FlatSpec
import scala.math._

class SnakeModel extends KaSpaceModel with FlatSpec
{
  // Agent radius
  val radius = 1

  // Site positions
  val posL = Position(-2, 0, 0)
  val posR = Position( 2, 0, 0)

  // Link orientations
  val w180 = Orientation()
  val w90LR = Orientation.z( Pi/2)
  val w90RL = Orientation.z(-Pi/2)

  // Contact graph
  contactGraph = s"A:{$radius}(l:{$posL}!{1}, r:{$posR}!{1}), 1:{$w180.$w180,$w90LR.$w90RL}"

  // Rules
  val r1 = "A(r), A(l)" -> s"A(r!1), A(l!1), 1:$w180.$w180  , A(l:$posL, r:$posR)" :@ 1
  val r2 = "A(r), A(l)" -> s"A(r!1), A(l!1), 1:$w90RL.$w90LR, A(l:$posL, r:$posR)" :@ 1

  // Observables
  withObs(s"A(r!1), A(l!1), 1:$w180.$w180", "straight links")
  withObs(s"A(r!1), A(l!1), 1:$w90LR.$w90RL", "90 deg turns")
  withObs(s"A(l:$posL, r:$posR)", "free A")

  // Mixture
  withInit(m"A(l:$posL, r:$posR)" * 2)

  // Simulate!
  withMaxEvents(500)
  run()
}

