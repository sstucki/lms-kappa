package kappa

import org.scalatest.FlatSpec
import scala.math._

class ProteasomeModel extends KaSpaceModel with FlatSpec
{
  // Agent radius
  val radius: Double = 0.8

  // Site positions
  val posL  = Position(-cos(Pi/7), sin(Pi/7), 0)
  val posR  = Position( cos(Pi/7), sin(Pi/7), 0)
  val posBL = Position(-1/2, 1/2 * tan(Pi/14), -1)
  val posBR = Position( 1/2, 1/2 * tan(Pi/14), -1)

  // Link orientations
  val wLR = Orientation.z(-2 * Pi / 7)
  val wRL = Orientation.z( 2 * Pi / 7)
  val wBL = Orientation.z(-Pi / 7) * Orientation.y(Pi)
  val wBR = Orientation.z( Pi / 7) * Orientation.y(Pi)

  // Contact graph
  contactGraph = s"""A:{$radius}(r:{$posR}!{1},
                                 l:{$posL}!{1},
                                 bl:{$posBL}!{2,2},
                                 br:{$posBR}!{3,3}),
                     1:{$wRL.$wLR},
                     2:{$wBL.$wBL},
                     3:{$wBR.$wBR}"""

  // Rules
  val bindLR = "A(r) , A(l) " -> s"A(r!1) , A(l!1),  1:$wRL.$wLR" :@ 1
  val bindBL = "A(bl), A(bl)" -> s"A(bl!1), A(bl!1), 1:$wBL.$wBL" :@ 1
  val bindBR = "A(br), A(br)" -> s"A(br!1), A(br!1), 1:$wBR.$wBR" :@ 1
  val unbindLR = "A(r!1) , A(l!1) " -> "A(r) , A(l) " :@ 1
  val unbindBL = "A(bl!1), A(bl!1)" -> "A(bl), A(bl)" :@ 1
  val unbindBR = "A(br!1), A(br!1)" -> "A(br), A(br)" :@ 1

  // Mixture
  withInit(m"A:$radius(l:$posL, r:$posR, bl:$posBL, br:$posBR)" * 50)

  // Expected observables
  withObs("A(r!1, br!2), A(l!1, bl!3), A(br!2, bl!3)", "side triangle")
  withObs("A(r!1), A(l!1)", "R-L")
  withObs("A(bl!1), A(bl!1)", "BL-BL")
  withObs("A(br!1), A(br!1)", "BR-BR")
  withObs("A(l!1), A(r!1, l!2), A(r!2, l!3), A(r!3, l!4), A(r!4, l!5)," +
    "A(r!5, l!6), A(r!6)", "7 chain")
  withObs("A(r!1, l!2), A(r!2, l!3), A(r!3, l!4), A(r!4, l!5), A(r!5, l!6)," +
    "A(r!6, l!7), A(r!7, l!1)", "7 ring")

  // Unexpected observables
  withObs("A(r!1, l!2), A(r!2, l!3), A(r!3, l!1)", "3 ring")
  withObs("A(r!1, bl!2), A(l!1, br!3), A(bl!2, br!3)", "wrong chirality triangle")
  withObs("A(l!1), A(r!1, l!2), A(r!2, l!3), A(r!3, l!4), A(r!4, l!5)," +
    "A(r!5, l!6), A(r!6, l!7), A(r!7)", "8 chain")

  // Simulate!
  withMaxTime(100.0)
  run
}

