package kappa.models

import scala.annotation.tailrec
import scala.language.postfixOps

import kappa.TKappaModel


class Chemosensors1dModel extends TKappaModel {

  // Model variants
  val withGrowingRing = true
  val withSensorBinding = true

  // Rate constant for the irreversible rules
  var insert = 2e-6
  var grow = 8e-6

  // Diffusion constant for chemoreceptors in the membrane
  val diffusion = 0.018 // units?

  // Length of a grid cell's side
  val h = 3.0 // units?
  val move = diffusion / (h * h)

  val bind = 0.01

  // Ising variables
  val alpha = 0.5
  val J = 5.0

  if (!withGrowingRing) {
    insert = 0.0
    grow = Double.PositiveInfinity
  }

  if (withSensorBinding) {

    // M is for Membrane: s is sensor, l is left and r is right
    // S is for Sensor: m is membrane, l is left and r is right
    contactGraph = "M(s!{1}, l!{2}, r!{2}), S(m!{1}, l!{3}, r!{3})"

    // Energy patterns
    energy (-J, "S(r!1), S(l!1)")
    energy (-alpha * J, "S(l, r)")

    withRules(
      // -- Chemosensor insertion --
      "M(s)" -> "M(s!1), S(m!1, l, r)" :@ insert,

      // -- Membrane growing --
      // "M(r!1), M(l!1)" -> "M(r!1), M(l!2), M(l!1, r!2, s)" :@ grow,
      // "M(s!1, r!2), S(m!1, r!5), M(s!4, l!3), S(m!4, l!5), M(l!2, r!3)" ->
      //   "M(s!1, r!2), S(m!1, r!4), M(s!3, l!2), S(m!3, l!4)" :@ Double.PositiveInfinity,
      "M(r!1, s  ), M(l!1, s  )" -> "M(r!1, s  ), M(l!2, s  ), M(l!1, r!2, s)" :@ grow,
      "M(r!1, s!_), M(l!1, s  )" -> "M(r!1, s!_), M(l!2, s  ), M(l!1, r!2, s)" :@ grow,
      "M(r!1, s  ), M(l!1, s!_)" -> "M(r!1, s  ), M(l!2, s!_), M(l!1, r!2, s)" :@ grow,

      "M(s!1, r!2), S(m!1, r), M(l!2, s!3), S(m!3, l)" ->
        "M(s!1, r!2), S(m!1, r), M(l!3, s!4), S(m!4, l), M(l!2, r!3, s)" :@ grow,
      // "M(s!1, r!2), S(m!1, r!4), M(s!3, l!2), S(m!3, l!4)" ->
      //   "M(s!1, r!2), S(m!1, r), M(s!4, l!3), S(m!4, l), M(l!2, r!3, s)" :@ grow,

      // -- Moving --
      "S(m!1, l, r), M(s!1, r!2), M(s, l!2)" <-> "S(m!1, l, r), M(s, r!2), M(s!1, l!2)" at move,

      // -- Binding --
      "M(s!1, r!2), S(m!1, r), M(s!3, l!2), S(m!3, l)" <-> "M(s!1, r!2), S(m!1, r!4), M(s!3, l!2), S(m!3, l!4)" at bind
    )

    val nonFreeL  = withObs ("NonFreeL", "S(l!_, r)")
    val nonFreeR  = withObs ("NonFreeR", "S(l, r!_)")
    val nonFreeRL = withObs ("NonFreeRL", "S(l!_, r!_)")
    val free = withObs ("Free", "S(l, r)")
    val link = withObs ("Link", "S(r!1), S(l!1)")

    // Invariants
    invariant (nonFreeL.inMix == nonFreeR.inMix)
    invariant (link.inMix * -J + free.inMix * alpha * -J == mixtureEnergy)

  } else {

    // M is for Membrane: l is left, r is right and s is sensor
    // S is for Sensor: m is membrane
    contactGraph = "M(s!{1}, l!{2}, r!{2}), S(m!{1})"

    // Energy patterns
    energy (-J, "M(s!1, r!2), S(m!1), M(s!3, l!2), S(m!3)")
    energy (-alpha * J, "M(s, r!1), M(s!2, l!1, r!3), S(m!2), M(s, l!3)")

    withRules(
      // -- Chemosensor insertion --
      "M(s)" -> "M(s!1), S(m!1)" :@ insert,

      // -- Membrane growing --
      // "M(r!1), M(l!1)" -> "M(r!1), M(l!2), M(l!1, r!2, s)" :@ grow,
      // "M(s!1, r!2), S(m!1, r!5), M(s!4, l!3), S(m!4, l!5), M(l!2, r!3)" ->
      //   "M(s!1, r!2), S(m!1, r!4), M(s!3, l!2), S(m!3, l!4)" :@ Double.PositiveInfinity,
      "M(r!1, s  ), M(l!1, s  )" -> "M(r!1, s  ), M(l!2, s  ), M(l!1, r!2, s)" :@ grow,
      "M(r!1, s!_), M(l!1, s  )" -> "M(r!1, s!_), M(l!2, s  ), M(l!1, r!2, s)" :@ grow,
      "M(r!1, s  ), M(l!1, s!_)" -> "M(r!1, s  ), M(l!2, s!_), M(l!1, r!2, s)" :@ grow,

      "M(s!1, r!2), S(m!1, r), M(l!2, s!3), S(m!3, l)" ->
        "M(s!1, r!2), S(m!1, r), M(l!3, s!4), S(m!4, l), M(l!2, r!3, s)" :@ grow,
      "M(s!1, r!2), S(m!1, r!4), M(s!3, l!2), S(m!3, l!4)" ->
        "M(s!1, r!2), S(m!1, r), M(s!4, l!3), S(m!4, l), M(l!2, r!3, s)" :@ grow,

      // -- Moving --
      "S(m!1), M(s!1, r!2), M(s, l!2)" <-> "S(m!1), M(s, r!2), M(s!1, l!2)" at move
    )
  }

  // Initial state
  withInit ("M(s, l!2, r!1), M(s, l!1, r!2)")


  // More invariants: no unbound sensors
  invariant ("S(m)".inMix == 0)


  // More observables
  val mem = withObs ("Mem", "M()")
  val sensor = withObs ("Sensor", "S()")


  // Perturbations
  if (!withGrowingRing) {
    when (mem.inMix >= 10000) exec {
      grow = 0
      insert = Double.PositiveInfinity
    } only 1 times

    when (sensor.inMix >= 2500) exec (insert = 0) only 1 times
  }

  if (withGrowingRing) {

    val realInsert = insert
    val realGrow = grow
    var stopTime: Option[Double] = None

    when (sensor.inMix >= 2000) exec {
      insert = 0
      grow = 0
      stopTime = Some(this.time)
      println("Stopped growing")
    } only 1 times

    when (stopTime.nonEmpty && this.time >= stopTime.get * 2) exec {
      insert = realInsert
      grow = realGrow
      println("Restarted growing")
    } only 1 times

    when (stopTime.nonEmpty && this.time >= stopTime.get * 3) stop
  }

  // Debug
  // @tailrec
  // final def sensors(a: Option[Mixture.Agent]): Unit = a match {
  //   case None => ()
  //   case Some(a) => {
  //     a.neighbour(0) match {
  //       case None => print(0)
  //       case _ => print(1)
  //     }
  //     sensors(a.neighbour(2))
  //   }
  // }

  if (withGrowingRing) {
    maxTime = 1.3e6
    // maxEvents = 800000
  } else {
    maxTime = 3e5
  }
  run
}

object Chemosensors1dModelMain {
  def main(args: Array[String]): Unit = new Chemosensors1dModel
}

