package kappa

import scala.language.implicitConversions
import scala.util.Random

trait Model extends Patterns with Mixtures with Actions with Rules
    with Perturbations with Parser with Symbols with Embeddings
    with PartialEmbeddings {
  self: LanguageContext =>

  var time      : Double               = 0
  var events    : Int                  = 0
  var nullEvents : Int                 = 0
  var maxTime   : Option[Double]       = None
  var maxEvents : Option[Int]          = None
  var obs       : Vector[Pattern]      = Vector()
  var obsNames  : Vector[String]       = Vector()

  def withInit(mix: Mixture) = { this.mix ++= mix; this }
  def withObs(obs: Pattern, description: String = "") = {
    val name = if (description == "") obs.toString else description
    this.obs = this.obs :+ obs
    this.obsNames = this.obsNames :+ name
    this
  }
  def withMaxTime(t: Double) = { maxTime = Some(t); this }
  def withMaxEvents(e: Int) = { maxEvents = Some(e); this }

  def runNormal(rand: Random) {

    // (Re-)compute all component embeddings.  This is necessary as
    // some components might have been registered before the mixture
    // was initialized (i.e. the LHS' of the rules).
    for (c <- patternComponents) {
      c.initEmbeddings
    }

    // Print the initial event number, time and the observable counts.
    println("" + events + "\t" + time + "\t" +
      (obs map (_.count)).mkString("\t"))

    // == Normal simulator main event loop ==
    while ((events < (maxEvents getOrElse (events + 1))) &&
      (time < (maxTime getOrElse Double.PositiveInfinity))) {

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

          // FIXME: We need to garbage collect embeddings of
          // observables for now.  This is really terribly inefficient
          // as it's linear in the number of embeddings (whether they
          // are consistent or not) :-(
          for (o <- obs) o.pruneEmbeddings

          // FIXME: This, on the other hand, should _NOT_ be
          // necessary.  Somehow the lazy embedding garbage collection
          // does not work as it should...
          for (r <- rules) r.action.lhs.pruneEmbeddings

          // Print the event number, time and the observable counts.
          println("" + events + "\t" + time + "\t" +
            (obs map (_.count)).mkString("\t"))
        } else {
          nullEvents += 1 // Null event
        }
      }
    }
  }

  def runNaive(rand: Random) {

    // Print the initial event number, time and the observable counts.
    println("" + events + "\t" + time + "\t" +
      (obs map (_.count)).mkString("\t"))

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
            (obs map (_.count)).mkString("\t"))
        } else {
          nullEvents += 1 // Null event
        }

      }
    }
  }

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

    println("# == Statistics:")
    println("#    Productive events : " + events)
    println("#    Null events       : " + nullEvents)
    println("#    Total time        : " + time)
    println

    println("# == K THX BYE!")
  }

  // Implicit conversions and other functions
  // FIXME: Why are these in a separate object?
  // RHZ: Just because I want to import only those after I create my model object
  // sstucki: but *everything* in Parser is in scope automatically anyway
  // because we mix in Parser into Model...
  // object HelperFns {
  //   implicit def stringToPattern(s: String) : Pattern = createPattern(s)

  //   def when(cond: => Boolean) = Cond(_ => cond)
  //   def when(cond: self.type => Boolean) = new Cond(cond)

  //   // TODO define every for "every 10 seconds set/add/del ..." or "every 10 events ..."
  // }
}

class KappaModel(val contactGraph: String) extends Model with KappaContext
with KappaParser with KappaSymbols {
  /*
  initSymbols(parseContactGraph(contactGraph) match {
    case Success(cg, _) => cg
    case msg => throw new IllegalArgumentException(
      "given contact graph is invalid: " + msg)
  })
  */
}
