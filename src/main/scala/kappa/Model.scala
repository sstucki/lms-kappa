package kappa

import scala.language.implicitConversions

trait Model extends Patterns with Mixtures with Actions with Rules
    with Perturbations with Parser with Symbols with Embeddings {
  self: LanguageContext =>

  var time      : Double               = 0
  var events    : Int                  = 0
  var maxTime   : Option[Double]       = None
  var maxEvents : Option[Int]          = None
  var obs       : Vector[Pattern]      = Vector()
  //val mix       : Mixture              = new Mixture() // Use Mixtures.mix

  def withInit(mix: Mixture) = { this.mix ++= mix; this }
  def withObs(obs: Pattern) = { this.obs = this.obs :+ obs; this }
  def withMaxTime(t: Double) = { maxTime = Some(t); this }
  def withMaxEvents(e: Int) = { maxEvents = Some(e); this }

  def run() {
    // FXIME: Naive simulator....

    println("# == Rules:")
    for (r <- rules) println("#    " + r)
    println
    println("# == Observables:")
    for (o <- obs) println("#    " + o)
    println

    // Create a new random number generator
    val rand = new util.Random

    println("# === Start of simulation ===")
    println("# Event\tTime\tObservables")

    // Main loop
    while (events < (maxEvents getOrElse (events + 1))) {

      // Collect all patterns (LHS' and observables)
      val lhss = for (i <- 0 until rules.length) yield (rules(i).action.lhs)
      val pats = lhss ++ obs

      // Compute all component embeddings.
      val embs =
        for (p <- pats) yield (p.components map (_ componentEmbeddingsIn mix))

      // Compute all observable counts.
      val obsCounts =
        for (i <- rules.size until rules.length + obs.length) yield {
          (embs(i) map (_.length)).product
        }

      println("" + events + "\t" + time + "\t" + obsCounts.mkString("\t"))

      // Compute all rule weights.
      val weights =
        for (i <- 0 until rules.size) yield {
          rules(i).rate((embs(i) map (_.length)): _*)
        }

      // Build the activity tree
      val tree = RandomTree(weights, rand)
      val totalActivity = tree.totalWeight

      if (totalActivity == 0) {
        println("# No more events. Terminating.")
        maxEvents = Some(0)
      } else {

        // Pick a rule and event at random
        val ri = tree.nextRandom._1
        val e = for (pes <- embs(ri)) yield pes(rand.nextInt(pes.size))
        val emb = new Embedding(e.toVector, rules(ri).action.lhs)

        // Apply the rule/event
        rules(ri).action(emb, mix)

        // Compute time advance
        val dt = -math.log(rand.nextDouble) / totalActivity
        time += dt
      }

      events += 1
    }

    println("# === End of simulation ===")
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
