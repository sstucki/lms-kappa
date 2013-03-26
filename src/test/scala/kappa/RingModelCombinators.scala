package kappa

import org.scalatest.FlatSpec
import scala.math._

class RingModelCombinators extends KaSpaceModel with FlatSpec {

  // Agent radius
  val radius = 1

  // Site positions
  val posL = Position.fromSpherical(2, Pi * 2 / 3, 0)
  val posR = Position.fromSpherical(2, Pi / 3, 0)

  // Link orientations
  val wLR = Orientation.z(-Pi * 2 / 3)
  val wRL = Orientation.z( Pi * 2 / 3)

  contactGraph = s"""A:{$radius} (c:{$posR}!{3:{$wRL}}, b:{$posL}!{1:{$wLR}}),
                     B:{$radius} (a:{$posR}!{1:{$wRL}}, c:{$posL}!{2:{$wLR}}),
                     C:{$radius} (b:{$posR}!{2:{$wRL}}, a:{$posL}!{3:{$wLR}})"""

  object A extends AgentType("A")
  object B extends AgentType("B")
  object C extends AgentType("C")
  val a = Site("a")
  val b = Site("b")
  val c = Site("c")

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

  println("totalNumMol = " + totalNumMol)
  println("totalConc = " + totalConc)
  println("volume = " + volume)
  println("on_rate = " + on_rate)
  println("off_rate = " + off_rate)
  println("close_rate = " + close_rate)


  // -- Rules --
  withRules(  // Bind
    A(b), B(a) -> A(b!1~wLR), B(a!1~wRL) :@ on_rate,
    B(c), C(b) -> B(c!1~wLR), C(b!1~wRL) :@ on_rate,
    C(a), A(c) -> C(a!1~wLR), A(c!1~wRL) :@ on_rate)

  withRules(  // Unbind
    A(b!1), B(a!1) -> A(b), B(a) :@ off_rate,
    B(c!1), C(b!1) -> B(c), C(b) :@ off_rate,
    C(a!1), A(c!1) -> C(a), A(c) :@ off_rate)

  withRules(  // Close
    B(a, c!1), C(b!1, a!2), A(c!2, b) ->
      B(a!3~wRL, c!1), C(b!1, a!2), A(c!2, b!3~wLR) :@ close_rate,
    C(b, a!1), A(c!1, b!2), B(a!2, c) ->
      C(b!3~wRL, a!1), A(c!1, b!2), B(a!2, c!3~wLR) :@ close_rate,
    A(c, b!1), B(a!1, c!2), C(b!2, a) ->
      A(c!3~wRL, b!1), B(a!1, c!2), C(b!2, a!3~wLR) :@ close_rate)


  // -- Mixture --
  withInit(nA)((A~radius)(b~posL, c~posR))
  withInit((B~radius)(a~posR, c~posL) * nB)
  withInit((C~radius)(a~posL, b~posR) * nC)


  // -- Expected observables --

  // Monomers
  withObs("A", A(b, c))
  withObs("B", B(a, c))
  withObs("C")(C(a, b))

  // Dimers
  withObs("AB")(A(b!1, c) :: B(a!1, c))
  withObs("AC")(A(c!1, b), C(a!1, b))
  withObs("BC")(B(c!1, a), C(b!1, a))

  // Trimers
  withObs("ABC")(A(b!1, c) :+ B(a!1, c!2) :+ C(a, b!2))
  withObs("BCA")(B(c!2, a) +: C(b!2, a!1) +: A(b, c!1))
  withObs("CAB")(C(a!1, b) +: A(c!1, b!2) :+ B(a!2, c))

  // Triangle
  withObs("triangle")(A(c!3, b!1), B(a!1, c!2), C(b!2, a!3))


  // -- Unexpected observables --

  // Hexagon
  withObs("hexagon")(
    A(c!6, b!1), B(a!1, c!2), C(b!2, a!3),
    A(c!3, b!4), B(a!4, c!5), C(b!5, a!6))

  // Tetramers
  withObs("ABCA")(A(b!1), B(a!1, c!2), C(b!2, a!3), A(c!3))
  withObs("BCAB")(B(c!2), C(b!2, a!3), A(c!3, b!1), B(a!1))
  withObs("CABC")(C(a!3), A(c!3, b!1), B(a!1, c!2), C(b!2))


  // Simulate!
  withMaxTime(1E6)
  //withMaxTime(1E8)
  run
}

