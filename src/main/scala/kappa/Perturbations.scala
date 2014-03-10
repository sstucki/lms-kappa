package kappa

import scala.language.implicitConversions

import java.io._


trait Perturbations {
  self: LanguageContext
      with Mixtures
      with Patterns
      with AbstractSyntax
      with Model =>

  import DoubleApprox._

  var mods : Vector[Perturbation] = Vector()

  object once
  object twice

  /** A trait for perturbations. */
  case class Perturbation(condition: PerturbationCondition,
                          effect: () => Unit)
      extends Function0[Unit] {

    var counter: Int = 0
    var maxCalls: Int = Int.MaxValue
    var until: () => Boolean = () => true
    var on = true

    def until(cond: => Boolean) {
      until = () => cond
    }

    def only(n: Int) = OnlyN(n)

    case class OnlyN(n: Int) {
      def times { maxCalls = counter + n }
    }

    def only(o: once.type) { maxCalls = counter + 1 }
    def only(t: twice.type) { maxCalls = counter + 2 }

    // -- Function0[Unit] API --

    /** Apply the effect if the condition holds. */
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
      extends SimulatorException("stopped by perturbation")

  // RHZ: Since the perturbation condition is an opaque function, we will probably
  //      have this problem: https://github.com/jkrivine/KaSim/issues/21

  /** A trait for perturbation conditions. */
  class PerturbationCondition(condition: () => Boolean)
      extends Function0[Boolean] {
    self =>

    def exec(effect: => Unit) = Perturbation(this, () => effect)

    class Add(n: Int) {
      def of(m: Mixture) = Perturbation(self, () => mix ++ m * n)
      def of(m: AbstractPattern): Perturbation = of(m.toMixture)
    }

    class Del(n: Int) {
      def of(p: Pattern) = Perturbation(self,
        () => removeNEmbeddings(n, p))
      def of(p: AbstractPattern): Perturbation = of(p.toPattern)
    }

    def add(n: Int) = new Add(n)
    def add(n: Int, m: Mixture) = new Add(n) of m
    def add(n: Int, m: AbstractPattern) = new Add(n) of m

    def del(n: Int) = new Del(n)
    def del(n: Int, p: Pattern) = new Del(n) of p
    def del(n: Int, p: AbstractPattern) = new Del(n) of p

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

    // RHZ: I find this a bit useless. WDYT?
    def snapshot(baseFilename: String) = {
      var n: Int = 1
      Perturbation(this, () => {
        val p = stringToPattern(mix.toString)
        val outputStream = new BufferedWriter(new FileWriter(
          baseFilename + "-" + n + ".snap"))

        // Count how many isomorphic copies of `c` there are in `cs`.
        def count(c: Pattern.Component, cs: Seq[Pattern.Component],
          rem: Seq[Pattern.Component], sum: Int):
            (Seq[Pattern.Component], Int) = cs match {
          case hd +: tl => if (c isEquivTo hd) count(c, tl, rem, sum+1)
                           else count(c, tl, hd +: rem, sum)
          case Nil => (rem, sum)
        }

        // Count how many connected components there are in each
        // isomorphism class in `cs`.
        def hist(cs: Seq[Pattern.Component]):
            Seq[(Pattern.Component, Int)] = cs match {
          case hd +: tl => {
            val (rem, sum) = count(hd, tl, List(), 1)
            (hd, sum) +: hist(rem)
          }
          case Nil => List()
        }

        for ((c, count) <- hist(p.components)) {
          outputStream.write(count + " * (" + c + ")")
          outputStream.newLine
        }
        outputStream.close
      })
    }

    def stop = Perturbation(this,
      () => throw StopPerturbationException)

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

