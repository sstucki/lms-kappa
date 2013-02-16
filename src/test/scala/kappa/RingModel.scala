package kappa

import org.scalatest.FlatSpec
import scala.math._

class RingModel extends KaSpaceModel with FlatSpec
{
  // Agent radius
  val radius = 1

  // Site positions
  val posL = Position.fromSpherical(2, Pi * 2 / 3, 0)
  val posR = Position.fromSpherical(2, Pi / 3, 0)

  // Link orientations
  val wLR = Orientation.y(-Pi * 2 / 3)
  val wRL = Orientation.y( Pi * 2 / 3)

  contactGraph = s"""A:$radius (b:{$posL}!{1}, c:{$posR}!{2}),
                     B:$radius (a:{$posR}!{1}, c:{$posL}!{3}),
                     C:$radius (a:{$posL}!{2}, b:{$posR}!{3}),
                     1:{$wLR.$wRL},
                     2:{$wRL.$wLR},
                     3:{$wLR.$wRL}"""

  // Rules
  val bindAB = "A(b), B(a)" -> "A(b!1), B(a!1), 1:" :@ 1
  val bindAC = "A(c), C(a)" -> "A(c!1), C(a!1), 1:" :@ 1
  val bindBC = "B(c), C(b)" -> "B(c!1), C(b!1), 1:" :@ 1
  val unbindAB = "A(b!1), B(a!1)" -> "A(b), B(a)" :@ 1
  val unbindAC = "A(c!1), C(a!1)" -> "A(c), C(a)" :@ 1
  val unbindBC = "B(c!1), C(b!1)" -> "B(c), C(b)" :@ 1

  // Mixture
  withInit(m"A:$radius (b:$posL, c:$posR)" * 50)
  withInit(m"B:$radius (a:$posR, c:$posL)" * 50)
  withInit(m"C:$radius (a:$posL, b:$posR)" * 50)

  // Simulate!
  withMaxEvents(100)
  run
}

