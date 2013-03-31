package kappa

import scala.language.implicitConversions

trait Perturbations {
  self: LanguageContext with Mixtures with Patterns with Model =>

  import DoubleApprox._

  var mods : Vector[Perturbation] = Vector()

  /** A trait for perturbations. */
  case class Perturbation(condition: PerturbationCondition,
                          effect: () => Unit)
      extends Function0[Unit] {

    var counter: Int = 0
    var maxCalls: Int = Int.MaxValue
    var until: () => Boolean = () => false
    var on = true

    def until(cond: => Boolean) {
      until = () => cond
    }

    def only(n: Int) = OnlyN(n)
    def only() = Only

    case class OnlyN(n: Int) {
      def times { maxCalls = counter + n }
    }

    object Only {
      def once { maxCalls = counter + 1 }
      def twice { maxCalls = counter + 2 }
    }

    // Function0[Unit] API

    def apply {
      if (on) {
        if (until() && counter < maxCalls) {
          if (condition()) {
            effect()
            counter += 1
          }
        } else {
          on = false
        }
      }
    }

    /** Register perturbation. */
    def register {
      mods = mods :+ this
    }

    // Add every instance of this class to the mods vector
    register
  }


  // -- Perturbation conditions --

  object StopPerturbationException
      extends Exception("stopped by perturbation")

  // RHZ: since the perturbation condition is an opaque function, we will probably
  //      have this problem: https://github.com/jkrivine/KaSim/issues/21

  /** A trait for perturbation conditions. */
  class PerturbationCondition(condition: () => Boolean)
      extends Function0[Boolean] {

    // Perturbation construction operators

    def exec(effect: => Unit) = Perturbation(this, () => effect)

    def add(n: Int) = Add(m => Perturbation(this, () => mix ++= m * n))
    def add(n: Int, m: Mixture) = Perturbation(this, () => mix ++ m * n)

    def removeNEmbeddings(n: Int, p: Pattern) {
      for (c <- p.components) {
        var i: Int = 0
        val u = c.head
        var v = mix.head
        while (i < n && v != null) {
          ComponentEmbedding.findEmbedding(u, v) match {
            case Some(inj) => { inj map (mix -= _); i += 1 }
            case None => ()
          }
          v = v.next
        }
      }
    }

    def del(n: Int) = Del(p => Perturbation(this, () => removeNEmbeddings(n, p)))
    def del(n: Int, p: Pattern) = Perturbation(this, () => removeNEmbeddings(n, p))

    // def snapshot(baseFilename: String) = Perturbation(this, ...)
    // def snapshot = Perturbation(this, () => emit(...))

    def stop = Perturbation(this,
      () => throw StopPerturbationException)

    case class Add(of: Mixture => Perturbation)
    case class Del(of: Pattern => Perturbation)

    // Function0[Boolean] API

    def apply: Boolean = condition()
  }

  final class TimeUnits(val lapse: Double) {
    def timeUnits: TimeUnits = this
    def seconds: TimeUnits = this
  }

  final class Events(val lapse: Int) {
    def events: Events = this
  }

  implicit def doubleToTU(t: Double): TimeUnits = new TimeUnits(t)
  implicit def intToE(n: Int): Events = new Events(n)

  def when(cond: => Boolean) = new PerturbationCondition(() => cond)

  def every(tu: TimeUnits) = {
    var m = 1
    new PerturbationCondition( () => {
      if (self.time >= tu.lapse * m) {m += 1; true}
      else false
    })
  }

  def every(e: Events) = new PerturbationCondition(
    () => self.events % e.lapse == 0)
}

