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
  val wLR = Orientation.z(-Pi * 2 / 3)
  val wRL = Orientation.z( Pi * 2 / 3)

  contactGraph = s"""A:{$radius} (c:{$posR}!{3}, b:{$posL}!{1}),
                     B:{$radius} (a:{$posR}!{1}, c:{$posL}!{2}),
                     C:{$radius} (b:{$posR}!{2}, a:{$posL}!{3}),
                     1:{$wLR.$wRL},
                     2:{$wLR.$wRL},
                     3:{$wRL.$wLR}"""

  // Number of molecules in initial mixture.
  val nA = 3000
  val nB = 3000
  val nC = 3000

  // Equilibrium constants
  val kD = 1E-12 // = beta / alpha1
  val kg = 1E-17 // = beta / (alpha1 + alpha2)

  // Deterministic rate constants
  val beta = 1E-6 // unbinding of any two monomers
  val alpha1 = beta / kD // binding of any two monomers
  val alpha2 = beta / kg - alpha1 // binding that leads to the formation of a cycle

  // TODO Compute the rates using alpha1 = 2.53E6 M^-1 s^-1
  // which is what they use in the paper.

  // Volume
  val totalNumMol = nA + nB + nC
  val totalConc = 400E-9 // = 400 nM
  val avogadro = 6.022E23 // molecules / mol
  // RHZ: volume should be divided by the avogadro number but since we are going
  // to multiply volume by that number in the next step we just don't do it.
  val volume = totalNumMol / totalConc // molecules L / moles

  // Stochastic rate constants
  val on_rate = alpha1 / volume // = 4.444E-5
  val off_rate = beta // = 1E-6
  val close_rate = alpha2 // = 1E11

  println("on_rate = " + on_rate)
  println("off_rate = " + off_rate)
  println("close_rate = " + close_rate)


  // Rules
  val bindAB = "A(b), B(a)" -> s"A(b!1), B(a!1), 1:$wLR.$wRL" :@ on_rate
  val bindAC = "A(c), C(a)" -> s"A(c!1), C(a!1), 1:$wRL.$wLR" :@ on_rate
  val bindBC = "B(c), C(b)" -> s"B(c!1), C(b!1), 1:$wLR.$wRL" :@ on_rate
  val unbindAB = "A(b!1), B(a!1)" -> "A(b), B(a)" :@ off_rate
  val unbindAC = "A(c!1), C(a!1)" -> "A(c), C(a)" :@ off_rate
  val unbindBC = "B(c!1), C(b!1)" -> "B(c), C(b)" :@ off_rate
  val closeAB = "B(a, c!1), C(b!1, a!2), A(c!2, b)" ->
    s"B(a!3, c!1), C(b!1, a!2), A(c!2, b!3), 3:$wRL.$wLR" :@ close_rate
  val closeAC = "A(c, b!1), B(a!1, c!2), C(b!2, a)" ->
    s"A(c!3, b!1), B(a!1, c!2), C(b!2, a!3), 3:$wRL.$wLR" :@ close_rate
  val closeBC = "C(b, a!1), A(c!1, b!2), B(a!2, c)" ->
    s"C(b!3, a!1), A(c!1, b!2), B(a!2, c!3), 3:$wRL.$wLR" :@ close_rate

  // Mixture
  withInit(m"A:$radius (b:$posL, c:$posR)" * nA)
  withInit(m"B:$radius (a:$posR, c:$posL)" * nB)
  withInit(m"C:$radius (a:$posL, b:$posR)" * nC)

  // -- Expected observables --

  // Monomers
  withObs("A(b, c)", "A")
  withObs("B(a, c)", "B")
  withObs("C(a, b)", "C")

  // Dimers
  withObs("A(b!1, c), B(a!1, c)", "AB")
  withObs("A(c!1, b), C(a!1, b)", "AC")
  withObs("B(c!1, a), C(b!1, a)", "BC")

  // Trimers
  withObs("A(b!1, c), B(a!1, c!2), C(a, b!2)", "ABC")
  withObs("B(c!2, a), C(b!2, a!1), A(b, c!1)", "BCA")
  withObs("C(a!1, b), A(c!1, b!2), B(a!2, c)", "CAB")

  // Triangle
  withObs("A(c!3, b!1), B(a!1, c!2), C(b!2, a!3)", "triangle")


  // -- Unexpected observables --

  // Hexagon
  withObs("A(c!6, b!1), B(a!1, c!2), C(b!2, a!3), " +
          "A(c!3, b!4), B(a!4, c!5), C(b!5, a!6)", "hexagon")

  // Tetramers
  withObs("A(b!1), B(a!1, c!2), C(b!2, a!3), A(c!3)", "ABCA")
  withObs("B(c!2), C(b!2, a!3), A(c!3, b!1), B(a!1)", "BCAB")
  withObs("C(a!3), A(c!3, b!1), B(a!1, c!2), C(b!2)", "CABC")


  // Simulate!
  withMaxTime(1E6)
  //withMaxTime(1E8)
  run
}
