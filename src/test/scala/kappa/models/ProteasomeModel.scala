package kappa.models

import org.scalatest.FlatSpec
import scala.math._

import kappa.{ KaSpaceModel, Orientation, Position }


class ProteasomeModel extends KaSpaceModel with FlatSpec {

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
  contactGraph = s"""A:{$radius}(r:{$posR}!{1:{$wRL}},
                                 l:{$posL}!{1:{$wLR}},
                                 bl:{$posBL}!{2:{$wBL},2:{$wBL}},
                                 br:{$posBR}!{3:{$wBR},3:{$wBR}})"""

  // Rules
  withRules(
    "A(r) , A(l) " -> s"A( r!1:$wRL), A( l!1:$wLR)" :@ 1,  // bind l-r
    "A(bl), A(bl)" -> s"A(bl!1:$wBL), A(bl!1:$wBL)" :@ 1,  // bind bl-bl
    "A(br), A(br)" -> s"A(br!1:$wBR), A(br!1:$wBR)" :@ 1,  // bind br-br
    "A(r!1) , A(l!1) " -> "A(r) , A(l) " :@ 1,             // unbind l-r
    "A(bl!1), A(bl!1)" -> "A(bl), A(bl)" :@ 1,             // unbind bl-bl
    "A(br!1), A(br!1)" -> "A(br), A(br)" :@ 1)             // unbind br-br

  // Mixture
  withInit(m"A:$radius(l:$posL, r:$posR, bl:$posBL, br:$posBR)" * 50)

  // Expected observables
  withObs("side triangle", "A(r!1, br!2), A(l!1, bl!3), A(br!2, bl!3)")
  withObs("R-L", "A(r!1), A(l!1)")
  withObs("BL-BL", "A(bl!1), A(bl!1)")
  withObs("BR-BR", "A(br!1), A(br!1)")
  withObs("7 chain", "A(l!1), A(r!1, l!2), A(r!2, l!3), " +
    "A(r!3, l!4), A(r!4, l!5), A(r!5, l!6), A(r!6)")
  withObs("7 ring", "A(r!1, l!2), A(r!2, l!3), A(r!3, l!4), " +
    "A(r!4, l!5), A(r!5, l!6), A(r!6, l!7), A(r!7, l!1)")

  // Unexpected observables
  withObs("3 ring", "A(r!1, l!2), A(r!2, l!3), A(r!3, l!1)")
  withObs("wrong chirality triangle",
    "A(r!1, bl!2), A(l!1, br!3), A(bl!2, br!3)")
  withObs("8 chain", "A(l!1), A(r!1, l!2), A(r!2, l!3), " +
    "A(r!3, l!4), A(r!4, l!5), A(r!5, l!6), A(r!6, l!7), A(r!7)")

  // Simulate!
  maxEvents = 100
  run
}

