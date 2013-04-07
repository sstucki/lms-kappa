package kappa.models

import kappa.TKappaModel

class Chemosensors1d extends TKappaModel {

  // Rate constant for the irreversible rules
  val insert = 2e-6
  val grow = 8e-6

  // Diffusion constant for chemoreceptors in the membrane
  val diffusion = 0.018 // units?

  // Length of a grid cell's side
  val h = 3.0 // units?
  val move = diffusion / (h * h)

  val bind = 0.01

  // Ising variables
  val alpha = 0.5
  val J = 5.0

  val withSensorBinding = true

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
      "M(r!1), M(l!1)" -> "M(r!1), M(l!2), M(l!1, r!2, s)" :@ grow,
      "M(s!1, r!2), S(m!1, r!5), M(s!4, l!3), S(m!4, l!5), M(l!2, r!3)" ->
        "M(s!1, r!2), S(m!1, r!4), M(s!3, l!2), S(m!3, l!4)" :@ Double.PositiveInfinity,

      // -- Moving --
      "S(m!1, l, r), M(s!1, r!2), M(s, l!2)" <-> "S(m!1, l, r), M(s, r!2), M(s!1, l!2)" at move,

      // -- Binding --
      "M(s!1, r!2), S(m!1, r), M(s!3, l!2), S(m!3, l)" <-> "M(s!1, r!2), S(m!1, r!4), M(s!3, l!2), S(m!3, l!4)" at bind
    )

    obs ("S(l!_, r)") named "NonFreeL"
    obs ("S(l, r!_)") named "NonFreeR"
    obs ("S(l!_, r!_)") named "NonFreeRL"
    val free = obs ("S(l, r)") named "Free"
    val link = obs ("S(r!1), S(l!1)") named "Link"

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
      "M(r!1), M(l!1)" -> "M(r!1), M(l!2), M(l!1, r!2, s)" :@ grow,

      // -- Moving --
      "S(m!1), M(s!1, r!2), M(s, l!2)" <-> "S(m!1), M(s, r!2), M(s!1, l!2)" at move
    )
  }

  // Initial state
  init ("M(s, l, r!1), M(s, l!1, r)")

  // More observables
  obs ("M()") named "M"
  obs ("S()") named "S"

  // Invariants
  // invariant ("S(s)".inMix == 0)
  // invariant ("M(l)".inMix == 1 && "M(r)".inMix == 1)

  maxTime = 1e6
  run
}

