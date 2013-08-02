package kappa

import scala.language.implicitConversions

import scala.util.Random


/** A class representing generic models. */
trait Model extends LanguageContext
    with ContactGraphs
    with SiteGraphs
    with Patterns
    with Mixtures
    with RollbackMachines
    with Embeddings
    with PartialEmbeddings
    with Actions
    with Rules
    with Perturbations
    with AbstractSyntax
    with Parsers {

  var time        : Double                = 0
  var events      : Long                  = 0
  var nullEvents  : Long                  = 0
  var _maxTime    : Option[Double]        = None
  var _maxEvents  : Option[Long]          = None
  var observables : Vector[Pattern]       = Vector()
  var obsNames    : Vector[String]        = Vector()
  var invariants  : Vector[() => Boolean] = Vector()


  /** Set the maximum time for the simulation. */
  def maxTime_=(t: Double) = { _maxTime = Some(t) }
  @inline def maxTime = _maxTime

  /** Set the maximum number of events for the simulation. */
  def maxEvents_=(e: Long) = { _maxEvents = Some(e) }
  @inline def maxEvents = _maxEvents


  // -- Invariants --

  def invariant(f: => Boolean) {
    this.invariants = this.invariants :+ (() => f)
  }


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


  // -- Observables --

  def withObs(description: String, obs: Pattern): Pattern = {
    val name = if (description == "") obs.toString else description
    this.observables = this.observables :+ obs
    this.obsNames = this.obsNames :+ name
    // TODO: See note above.
    for (cc <- obs.components) cc.register
    obs
  }
  def withObs(obs: Pattern): Pattern = withObs("", obs)
  def withObs(description: String, obs: AbstractPattern): Pattern =
    withObs(description, obs.toPattern)
  def withObs(obs: AbstractPattern): Pattern = withObs("", obs.toPattern)
  def withObs(description: String, obs: PartialAbstractPattern*): Pattern =
    withObs(description, partialAbstractPatternsToPattern(obs))
  def withObs(obs: PartialAbstractPattern*): Pattern = withObs("", obs: _*)

  // FIXME: withObs(description: String) shadows
  // withObs(obs: AbstractPattern)
  // "Curried" version of `withObs`.
  // final class WithObs(description: String) {
  //   def apply(obs: Pattern): Pattern = withObs(description, obs)
  //   def apply(obs: PartialAbstractPattern*): Pattern =
  //     withObs(description, obs: _*)
  // }
  // def withObs(description: String) = new WithObs(description)


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
    implicit ab: ActionBuilder, bab: BiActionBuilder)
      : RuleBox = {
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
    implicit ab: ActionBuilder, bab: BiActionBuilder)
      : Seq[RuleBox] = {
    val rs = partialAbstractRulesToRules(rh :: rt.toList)
    rs map registerRule
    rs
  }


  // -- Simulation and trajectories --

  /**
   * An exception that can be thrown in order to exit the simulator
   * event loop.
   */
  class SimulatorException(val msg: String) extends Exception(msg)

  case object Deadlock extends SimulatorException("no more events")
  case object EmptyRuleSet extends SimulatorException("no rules")
  case class InvariantViolation(override val msg: String)
      extends SimulatorException(msg)

  // Create a new random number generator
  val rand = new util.Random

  // To handle the case when the apparent total activity is > 0 but
  // the real total activity is 0 (due to the square approximation)
  var maxConsecutiveClashes: Int = 20

  def initialise {

    if (rules.length == 0) {
      throw EmptyRuleSet
    }

    Mixture.initialiseLinkIds

    // (Re-)compute all component embeddings.  This is necessary as
    // some components might have been registered before the mixture
    // was initialized (e.g. the LHS of the rules).
    for (c <- patternComponents)
      c.initEmbeddings
  }

  def run {

    initialise

    // Print model info
    println("# == Rules:")
    for (r <- rules) {
      println("#    " + r)
      println("#      " +  r.action.atoms)
    }
    println
    println("# == Observables:")
    for ((n, o) <- obsNames zip observables)
      println("#    " + n + ": " + o)
    println

    println("# === Start of simulation ===")
    println("Event\tTime\t" + obsNames.mkString("\"", "\"\t\"", "\""))

    println("" + events + "\t" + time + "\t" +
      (observables map (_.inMix)).mkString("\t"))

    var consecutiveClashes: Int = 0

    while ((events < (maxEvents getOrElse (events + 1))) &&
           (time < (maxTime getOrElse Double.PositiveInfinity))) {

      // TODO Perhaps we should even distinguish between "hard" and
      // "soft" deadlock... or perhaps we should abandon the square
      // approximation here and only return throw hard deadlocks
      if (consecutiveClashes >= maxConsecutiveClashes)
        throw Deadlock

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
      // FIXME: We should perhaps handle this more elegantly
      val (productive, clash) = r.action(e, mix)
      if (productive) {

        events += 1 // Productive event

        // Print the event number, time and the observable counts.
        println("" + events + "\t" + time + "\t" +
          (observables map (_.inMix)).mkString("\t"))

        consecutiveClashes = 0

      } else {
        nullEvents += 1 // Null event

        if (clash)
          consecutiveClashes += 1
      }

      // Check invariants
      if (!invariants.forall(_()))
        throw new InvariantViolation("rule " + r + " violated invariant")

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
}

