package kappa

class KaSpaceModel extends Model
  with KaSpaceContext with KaSpaceActions with KaSpaceParser
  with KaSpaceContactGraph
{
  override def run()
  {
    // Run geometric soundness check to initialize
    // position/orientation of all agents in the mixture.
    print("# == Initialize mixture geometry...")
    if(!KaSpaceAction.checkGeometricSoundness(mix)) {
      println(" failed!")
      println("# == Geometrically unsound initial mixture. Terminating...")
    } else {
      println(" done.")

      // Run the standard simulator.
      super.run
    }
  }
}

