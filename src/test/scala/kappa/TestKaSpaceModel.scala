package kappa

import org.scalatest.FlatSpec
import scala.math._

class ProteasomeModel extends KaSpaceModel with FlatSpec
{
  // Agent radius
  val radius: Double = 1

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
  val bindBL = "A(bl), A(bl)" -> s"A(bl!1), A(bl!1), 2:$wBL.$wBL" :@ 1
  val bindBR = "A(br), A(br)" -> s"A(br!1), A(br!1), 3:$wBR.$wBR" :@ 1
  val unbindLR = "A(r!1) , A(l!1) " -> "A(r) , A(l) " :@ 1
  val unbindBL = "A(bl!1), A(bl!1)" -> "A(bl), A(bl)" :@ 1
  val unbindBR = "A(br!1), A(br!1)" -> "A(br), A(br)" :@ 1

  // Mixture
  withInit(m"A:$radius(l:$posL, r:$posR, bl:$posBL, br:$posBR)" * 50)

  // Simulate!
  withMaxTime(10.0)
  run
}

