package kappa

import scala.language.implicitConversions

trait Model extends LanguageContext with Patterns with Perturbations with Parser {
  self =>

  // Mixture
  class Mixture() { // dummy
    def ++(that: Mixture) : Mixture = this
  }

  var time      : Double               = 0
  var events    : Int                  = 0
  var maxTime   : Option[Double]       = None
  var maxEvents : Option[Int]          = None
  var init      : Mixture              = new Mixture()
  var obs       : Vector[Pattern]      = Vector()

  def withInit(mix: Mixture) = { init ++= mix; this }
  def withMaxTime(t: Double) = { maxTime = Some(t); this }
  def withMaxEvents(e: Int) = { maxEvents = Some(e); this }

  def run() { }

  // Implicit conversions and other functions
  object HelperFns {
    implicit def stringToPattern(s: String) : Pattern = createPattern(s)

    def when(cond: => Boolean) = Cond(_ => cond)
    def when(cond: self.type => Boolean) = new Cond(cond)

    // TODO define every for "every 10 seconds set/add/del ..." or "every 10 events ..."
  }
}

class KappaModel(val contactGraph: String) extends Model with KappaParser {
  val env : Env = createEnv(parseContactGraph(contactGraph) match {
    case Success(cg, _) => cg
    case msg => println(msg); println();
                throw new IllegalArgumentException("given contact graph is invalid")
  })
}

// Testing
object Test {
  def main(args: Array[String]) {
    val model = new KappaModel("A(s:{p,q}!{1,1})") // contact graph
    println("Contact graph = " + model.contactGraph)
    import model.HelperFns._

    var k = 5
    val r1 = "A(s), A(s)" -> "A(s!1), A(s!1)" :@ k * 2
    println(r1)
    k = 6
    println(r1)

    val r2 = "A(s!1), A(s!1)" -> "A(s), A(s)" !@ { ccs => k += 1; ccs(0) * 10 } // is there a way to do this with :@

    // Quasi-steady-state approximation
    val vmax = 1
    val km = 1
    val r3 = "A(s:p)" -> "A(s:q)" !@ (ccs => vmax * ccs(0) / (km + ccs(0)))

    val m1 = when ("A(s)".inMix < 10) set (k = 7)
    val m2 = when (10 > "A(s)".inMix) set println("k = 7")

    val m3 = when (_.time >= 10) add 50 of "A(s)"
    // NB: since the perturbation condition is an opaque function, we will probably have
    //     this problem: https://github.com/jkrivine/KaSim/issues/21

    val m4 = when (_.events == 1000) del 50 of "A(s)"

    println(model.rules)

    model.run
  }
}

