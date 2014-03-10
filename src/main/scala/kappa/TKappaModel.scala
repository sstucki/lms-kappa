package kappa

/** A class representing Kappa models. */
abstract class TKappaModel extends Model
    with KappaContext
    with KappaParsers
    with KappaAbstractSyntax
    with TKappaActions {

  var temperature: Double = 298.15 // room temperature
  var energyPatterns: Array[(Pattern.Component, Double)] = Array()
  val kB: Double = 1.3806488e-23


  /** Compute the current energy of the system. */
  def mixtureEnergy: Double =
    (for ((p, c) <- energyPatterns) yield (c * p.inMix)).sum

  /** Declare an energy pattern. */
  def energy(energyCost: Double, pattern: Pattern) {

    if (pattern.components.length > 1)
      throw new IllegalArgumentException("energy pattern is disconnected")

    val cc = pattern.components(0)
    val epIndex = energyPatterns indexWhere {
      case (p, c) => p isEquivTo cc }

    if (epIndex < 0) { // no iso found
      cc.register
      energyPatterns = energyPatterns :+ (cc, energyCost)
    } else { // iso found
      val (p, c) = energyPatterns(epIndex)
      println("# Found isomorphic energy pattern: " + p)
      println("#   Updating its energy value from " + c + " to " +
        (c + energyCost))
      energyPatterns(epIndex) = (p, c + energyCost)
    }
  }

  /** Declare an energy pattern. */
  def energy(energyCost: Double, pattern: AbstractPattern) {
    energy(energyCost, pattern.toPattern)
  }

  /** Declare an energy pattern. */
  def energy(energyCost: Double, pattern: PartialAbstractPattern*) {
    energy(energyCost, partialAbstractPatternsToPattern(pattern))
  }
}

