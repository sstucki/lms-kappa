package kappa

import scala.collection.mutable

import scala.language.implicitConversions


class KaSpaceModel extends Model with KaSpaceContext with KaSpaceActions
    with KaSpaceAbstractSyntax with KaSpaceParser {

  /** Run a simulation of this model. */
  override def run() {

    // Run geometric soundness check to initialize
    // position/orientation of all agents in the mixture.
    print("# == Initialize mixture geometry...")
    if(!KaSpaceActionBuilder.checkGeometricSoundness(mix)) {
      println(" failed!")
      println("# == Geometrically unsound initial mixture. Terminating...")
    } else {
      println(" done.")

      // Run the standard simulator.
      super.run
    }
  }
}

