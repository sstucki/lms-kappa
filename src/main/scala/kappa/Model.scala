package kappa

import scala.language.implicitConversions
import scala.util.Random

/** A class representing generic models. */
trait Model extends Agents with Patterns
  with Mixtures with Actions with Rules with Perturbations
  with Parser with Symbols with Embeddings with PartialEmbeddings
{
  self: LanguageContext =>

  var time      : Double               = 0
  var events    : Long                 = 0
  var nullEvents : Long                = 0
  var maxTime   : Option[Double]       = None
  var maxEvents : Option[Long]         = None
  var obs       : Vector[Pattern]      = Vector()
  var obsNames  : Vector[String]       = Vector()

  def withInit(mix: Mixture): Model             = { this.mix ++= mix; this }
  def withInit(mix: Mixture, count: Int): Model = withInit(mix * count)
  def withInit(mix: Pattern): Model             = withInit(Mixture(mix))
  def withInit(mix: Pattern, count: Int): Model = withInit(Mixture(mix) * count)

  def withObs(obs: Pattern, description: String = "") = {
    val name = if (description == "") obs.toString else description
    this.obs = this.obs :+ obs
    this.obsNames = this.obsNames :+ name
    this
  }

  def withMaxTime(t: Double) = { maxTime = Some(t); this }
  def withMaxEvents(e: Long) = { maxEvents = Some(e); this }

  var _contactGraph: String = ""
  def contactGraph: String = _contactGraph
  def contactGraph_= (cg: String) = {
    _contactGraph = cg
    initSymbols(parseContactGraph(cg))
  }

  def runNormal(rand: Random) {

    // (Re-)compute all component embeddings.  This is necessary as
    // some components might have been registered before the mixture
    // was initialized (i.e. the LHS' of the rules).
    for (c <- patternComponents) {
      c.initEmbeddings
    }

    // Print the initial event number, time and the observable counts.
    println("" + events + "\t" + time + "\t" +
      (obs map (_.inMix)).mkString("\t"))

    // == Normal simulator main event loop ==
    while ((events < (maxEvents getOrElse (events + 1))) &&
      (time < (maxTime getOrElse Double.PositiveInfinity))) {

      // Compute all rule weights.
      val weights = for (r <- rules) yield r.law()

      // Build the activity tree
      val tree = RandomTree(weights, rand)
      val totalActivity = tree.totalWeight

      if ((totalActivity == 0) || (totalActivity.isNaN)) {
        println("# No more events. Terminating.")
        maxEvents = Some(0)
      } else {

        // Advance time
        val dt = -math.log(rand.nextDouble) / totalActivity
        time += dt

        // Pick a rule and event at random
        val r = rules(tree.nextRandom._1)
        val e = r.action.lhs.randomEmbedding(rand)

        // Apply the rule/event
        if (r.action(e, mix)) {
          events += 1 // Productive event

          // Print the event number, time and the observable counts.
          println("" + events + "\t" + time + "\t" +
            (obs map (_.inMix)).mkString("\t"))
        } else {
          nullEvents += 1 // Null event
        }
      }
    }
  }

  def runNaive(rand: Random) {

    // Print the initial event number, time and the observable counts.
    println("" + events + "\t" + time + "\t" +
      (obs map (_.inMix)).mkString("\t"))

    // == Naive simulator main event loop ==
    while ((events < (maxEvents getOrElse (events + 1))) &&
      (time < (maxTime getOrElse Double.PositiveInfinity))) {

      // (Re-)compute all component embeddings.
      for (c <- patternComponents) {
        c.initEmbeddings
      }

      // Compute all rule weights.
      val weights = for (r <- rules) yield r.law()

      // Build the activity tree
      val tree = RandomTree(weights, rand)
      val totalActivity = tree.totalWeight

      if (totalActivity == 0) {
        println("# No more events. Terminating.")
        maxEvents = Some(0)
      } else {

        // Advance time
        val dt = -math.log(rand.nextDouble) / totalActivity
        time += dt

        // Pick a rule and event at random
        val r = rules(tree.nextRandom._1)
        val e = r.action.lhs.randomEmbedding(rand)

        // Apply the rule/event
        if (r.action(e, mix)) {
          events += 1 // Productive event

          // Print the event number, time and the observable counts.
          println("" + events + "\t" + time + "\t" +
            (obs map (_.inMix)).mkString("\t"))
        } else {
          nullEvents += 1 // Null event
        }

      }
    }
  }

  /** Run a simulation of this model. */
  def run() {

    // Create a new random number generator
    val rand = new util.Random

    // Print model info
    println("# == Rules:")
    for (r <- rules) println("#    " + r + ": " + r.action.atoms)
    println
    println("# == Observables:")
    for ((n, o) <- obsNames zip obs) println("#    " + n + ": " + o)
    println

    println("# === Start of simulation ===")
    println("Event\tTime\t" + obsNames.mkString("\"", "\"\t\"", "\""))

    // Call normal main loop
    runNormal(rand)
    //runNaive(rand)

    println("# === End of simulation ===")
    println

    val totalEvents: Double = events + nullEvents
    println("# == Statistics:")
    println("#    Productive events : " + events)
    println("#    Null events       : " + nullEvents)
    println("#    Efficiency        : " + events / totalEvents)
    println("#    Total time        : " + time)
    println

    println("# == K THX BYE!")
  }
}

