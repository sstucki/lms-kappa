package kappa

import scala.language.implicitConversions
import scala.util.Random

/** A class representing generic models. */
trait Model extends LanguageContext with ContactGraphs with SiteGraphs
    with Patterns with Mixtures with Embeddings with PartialEmbeddings
    with Actions with Rules with Perturbations with AbstractSyntax
    with Parsers {

  var time      : Double               = 0
  var events    : Long                 = 0
  var nullEvents : Long                = 0
  var maxTime   : Option[Double]       = None
  var maxEvents : Option[Long]         = None
  var obs       : Vector[Pattern]      = Vector()
  var obsNames  : Vector[String]       = Vector()

  def withInit(mix: Mixture): Model             = { this.mix ++= mix; this }
  def withInit(count: Int, mix: Mixture): Model = withInit(mix * count)
  def withInit(mix: Pattern): Model             = withInit(Mixture(mix))
  def withInit(count: Int, mix: Pattern): Model = withInit(Mixture(mix) * count)
  def withInit(mix: AbstractPattern): Model     = withInit(Mixture(mix.toPattern))
  def withInit(count: Int, mix: AbstractPattern): Model =
    withInit(Mixture(mix.toPattern) * count)
  def withInit(mix: PartialAbstractPattern*): Model =
    withInit(partialAbstractPatternsToMixture(mix))
  def withInit(count: Int, mix: PartialAbstractPattern*): Model =
    withInit(partialAbstractPatternsToMixture(mix) * count)

  // "Curried" version of `withInit`.
  final class WithInit(count: Int) {
    def apply(mix: Mixture): Model = withInit(count, mix)
    def apply(mix: Pattern): Model = withInit(count, mix)
    def apply(mix: PartialAbstractPattern*): Model = withInit(count, mix: _*)
  }
  def withInit(count: Int) = new WithInit(count)

  def withObs(description: String, obs: Pattern): Model = {
    val name = if (description == "") obs.toString else description
    this.obs = this.obs :+ obs
    this.obsNames = this.obsNames :+ name
    this
  }
  def withObs(obs: Pattern): Model = withObs("", obs)
  def withObs(description: String, obs: AbstractPattern): Model =
    withObs(description, obs.toPattern)
  def withObs(obs: AbstractPattern): Model = withObs("", obs)
  def withObs(description: String, obs: PartialAbstractPattern*): Model =
    withObs(description, partialAbstractPatternsToPattern(obs))
  def withObs(obs: PartialAbstractPattern*): Model = withObs("", obs: _*)

  // "Curried" version of `withObs`.
  final class WithObs(description: String) {
    def apply(obs: Pattern): Model = withObs(description, obs)
    def apply(obs: PartialAbstractPattern*): Model =
      withObs(description, obs: _*)
  }
  def withObs(description: String) = new WithObs(description)


  def withMaxTime(t: Double) = { maxTime = Some(t); this }
  def withMaxEvents(e: Long) = { maxEvents = Some(e); this }


  /** Register a single rule in the model. */
  def withRule(r: Rule): Rule = { registerRule(r); r }

  /** Register a sequence of rules in the model. */
  def withRules(rs: Rule*): Seq[Rule] = {
    rs map registerRule
    rs.toList
  }

  /**
   * Convert a non-empty sequence of partial abstract rules into a
   * single rule and register it.
   */
  def withRule(
    rh: PartialAbstractRule, rt: PartialAbstractRule*)(
    implicit ab: ActionBuilder): Rule = {
    val rs = partialAbstractRulesToRules(rh :: rt.toList)
    if (rs.length != 1) {
      throw new IllegalArgumentException(
        "expected a single rule specification but found multiple")
    } else {
      val r = rs.head
      registerRule(r)
      r
    }
  }

  /**
   * Convert a non-empty sequence of partial abstract rules into a
   * sequence of rules and register them.
   */
  def withRules(
    rh: PartialAbstractRule, rt: PartialAbstractRule*)(
    implicit ab: ActionBuilder): Seq[Rule] = {
    val rs = partialAbstractRulesToRules(rh :: rt.toList)
    rs map registerRule
    rs
  }


  // -- Simulation functions --

  /**
   * An exception that can be thrown in order to exit the simulator
   * event loop.
   */
  case class SimulatorException(msg: String) extends Exception(msg)


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

    try {

      // == Normal simulator main event loop ==
      while ((events < (maxEvents getOrElse (events + 1))) &&
        (time < (maxTime getOrElse Double.PositiveInfinity))) {

        if (rules.length == 0) {
          throw SimulatorException("no rules")
        }

        // Compute all rule weights.
        val weights = for (r <- rules) yield r.law()

        // Build the activity tree
        val tree = RandomTree(weights, rand)
        val totalActivity = tree.totalWeight

        if ((totalActivity == 0) || (totalActivity.isNaN)) {
          throw SimulatorException("no more events")
        }

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
    } catch {
      case SimulatorException(msg) => {
        println("# Interrupted simulation: " + msg + ". Terminating.")
      }
    }
  }

  def runNaive(rand: Random) {

    // Print the initial event number, time and the observable counts.
    println("" + events + "\t" + time + "\t" +
      (obs map (_.inMix)).mkString("\t"))

    try {
      // == Naive simulator main event loop ==
      while ((events < (maxEvents getOrElse (events + 1))) &&
        (time < (maxTime getOrElse Double.PositiveInfinity))) {

        if (rules.length == 0) {
          throw SimulatorException("no rules")
        }

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
          throw SimulatorException("no more events")
        }

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
    } catch {
      case SimulatorException(msg) => {
        println("# Interrupted simulation: " + msg + ". Terminating.")
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

