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

  contactGraph = s"""A:{$radius} (b:{$posL}!{1}, c:{$posR}!{2}),
                     B:{$radius} (a:{$posR}!{1}, c:{$posL}!{3}),
                     C:{$radius} (a:{$posL}!{2}, b:{$posR}!{3}),
                     1:{$wLR.$wRL},
                     2:{$wRL.$wLR},
                     3:{$wLR.$wRL}"""

  // Rate constants
  val on_rate = 1
  val off_rate = 1
  val close_rate = 10

  // Rules
  val bindAB = "A(b), B(a)" -> s"A(b!1), B(a!1), 1:$wLR.$wRL" :@ on_rate
  val bindAC = "A(c), C(a)" -> s"A(c!1), C(a!1), 1:$wRL.$wLR" :@ on_rate
  val bindBC = "B(c), C(b)" -> s"B(c!1), C(b!1), 1:$wLR.$wRL" :@ on_rate
  val unbindAB = "A(b!1), B(a!1)" -> "A(b), B(a)" :@ off_rate
  val unbindAC = "A(c!1), C(a!1)" -> "A(c), C(a)" :@ off_rate
  val unbindBC = "B(c!1), C(b!1)" -> "B(c), C(b)" :@ off_rate
  val closeAB = "A(b, c!1), B(a, c!2), C(a!1, b!2)" ->
    s"A(b!3, c!1), B(a!3, c!2), C(a!1, b!2), 1:$wLR.$wRL" :@ close_rate
  val closeAC = "A(c, b!1), B(a!1, c!2), C(a, b!2)" ->
    s"A(c!3, b!1), B(a!1, c!2), C(a!3, b!2), 1:$wRL.$wLR" :@ close_rate
  val closeBC = "A(b!1, c!2), B(a!1, c), C(a!2, b)" ->
    s"A(b!1, c!2), B(a!1, c!3), C(a!2, b!3), 1:$wLR.$wRL" :@ close_rate

  // Expected observables
  withObs("A(c!3, b!1), B(a!1, c!2), C(b!2, a!3)", "triangle")
  withObs("A(b!1), B(a!1)", "A-B")
  withObs("A(c!1), C(a!1)", "A-C")
  withObs("B(c!1), C(b!1)", "B-C")
  withObs("A(b!1), B(a!1, c!2), C(b!2)", "A-B-C")

  // Unexpected observables
  withObs("A(b!1), B(a!1, c!2), C(b!2, a!3), A(c!3)", "A-B-C-A")
  withObs("A(c!6, b!1), B(a!1, c!2), C(b!2, a!3), A(c!3, b!4), "
    + "B(a!4, c!5), C(b!5, a!6)", "hexagon")

  // Mixture
  withInit(m"A:$radius (b:$posL, c:$posR)" * 200)
  withInit(m"B:$radius (a:$posR, c:$posL)" * 200)
  withInit(m"C:$radius (a:$posL, b:$posR)" * 200)

  // Simulate!
  withMaxEvents(10000)
  run
}

