package kappa

import scala.language.implicitConversions

import scala.util.Random


/** A class representing generic models. */
trait Model extends LanguageContext with ContactGraphs with SiteGraphs
    with Patterns with Mixtures with Embeddings with PartialEmbeddings
    with Actions with Rules with Perturbations with AbstractSyntax
    with Parsers {

  var time        : Double               = 0
  var events      : Long                 = 0
  var nullEvents  : Long                 = 0
  var _maxTime    : Option[Double]       = None
  var _maxEvents  : Option[Long]         = None
  var observables : Vector[Pattern]      = Vector()
  var obsNames    : Vector[String]       = Vector()


  /** Set the maximum time for the simulation. */
  def maxTime_=(t: Double) = { _maxTime = Some(t) }
  @inline def maxTime = _maxTime

  /** Set the maximum number of events for the simulation. */
  def maxEvents_=(e: Long) = { _maxEvents = Some(e) }
  @inline def maxEvents = _maxEvents


  // -- Initial state --

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


  final class Count(count: Int) {
    def of(mix: Mixture) = mix * count
    def of(mix: Pattern) = Mixture(mix) * count
    def of(mix: AbstractPattern) =
      Mixture(mix.toPattern) * count
    def of(mix: PartialAbstractPattern*) =
      partialAbstractPatternsToMixture(mix) * count

    def *(mix: Mixture) = of(mix)
    def *(mix: Pattern) = of(mix)
    def *(mix: AbstractPattern) = of(mix)
    def *(mix: PartialAbstractPattern*) = of(mix: _*)
  }

  implicit def intToCount(n: Int) = new Count(n)

  /** Declare a mixture as part of the initial state. */
  def init(mix: Mixture) { this.mix ++= mix }
  def init(mix: Pattern) { init(Mixture(mix)) }
  def init(mix: AbstractPattern) { init(Mixture(mix.toPattern)) }
  def init(mix: PartialAbstractPattern*) {
    init(partialAbstractPatternsToMixture(mix))
  }


  // -- Observables --

  final class Obs(obs: Pattern) {
    def named(name: String) {
      obsNames = obsNames.dropRight(1) :+ name
    }

    observables = observables :+ obs
    obsNames = obsNames :+ (obs.toString)
  }

  /** Declare an observable. */
  def obs(obs: Pattern) = new Obs(obs)
  def obs(obs: AbstractPattern) = new Obs(obs.toPattern)
  def obs(obs: PartialAbstractPattern*) =
    new Obs(partialAbstractPatternsToPattern(obs))


  def withObs(description: String, obs: Pattern): Model = {
    val name = if (description == "") obs.toString else description
    this.observables = this.observables :+ obs
    this.obsNames = this.obsNames :+ name
    this
  }
  def withObs(obs: Pattern): Model = withObs("", obs)
  def withObs(description: String, obs: AbstractPattern): Model =
    withObs(description, obs.toPattern)
  def withObs(obs: AbstractPattern): Model = withObs("", obs.toPattern)
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


  // -- Rules --

  /** Register a single rule in the model. */
  def withRule(r: RuleBox): RuleBox = { registerRule(r); r }

  /** Register a sequence of rules in the model. */
  def withRules(rs: RuleBox*): Seq[RuleBox] = {
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

  object Deadlock extends SimulatorException("no more events")
  object EmptyRuleSet extends SimulatorException("no rules")

  def run() {

    if (rules.length == 0) {
      throw EmptyRuleSet
    }

    Mixture.initialiseLinkIds

    // Create a new random number generator
    val rand = new util.Random

    // (Re-)compute all component embeddings.  This is necessary as
    // some components might have been registered before the mixture
    // was initialized (i.e. the LHS' of the rules).
    //
    // TODO: To not compute twice we should not compute them when
    // registering patterns.
    for (c <- patternComponents) {
      c.initEmbeddings
    }

    println("# === Start of simulation ===")

    println("" + events + "\t" + time + "\t" +
      (observables map (_.inMix)).mkString("\t"))

    while ((events < (maxEvents getOrElse (events + 1))) &&
      (time < (maxTime getOrElse Double.PositiveInfinity))) {

      // Compute all rule weights.
      val weights = for (r <- rules) yield r.law()

      // Build the activity tree
      val tree = RandomTree(weights, rand)
      val totalActivity = tree.totalWeight

      if ((totalActivity == 0) || (totalActivity.isNaN)) {
        throw Deadlock
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
          (observables map (_.inMix)).mkString("\t"))
      } else {
        nullEvents += 1 // Null event
      }

      // Apply perturbations
      for (mod <- mods) mod()
    }

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

  def runUntilDeadlock() {
    try {
      run()
    } catch {
      case Deadlock => ()
    }
  }

  /*
  def runNormal(rand: Random) {

    // (Re-)compute all component embeddings.  This is necessary as
    // some components might have been registered before the mixture
    // was initialized (i.e. the LHS' of the rules).
    //
    // TODO: To not compute twice we should not compute them when
    // registering patterns.
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
          throw EmptyRuleSet
        }

        // Compute all rule weights.
        val weights = for (r <- rules) yield r.law()

        // Build the activity tree
        val tree = RandomTree(weights, rand)
        val totalActivity = tree.totalWeight

        if ((totalActivity == 0) || (totalActivity.isNaN)) {
          throw Deadlock
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

  // RHZ: Shouldn't we get rid of this at some point?
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
  */
}

