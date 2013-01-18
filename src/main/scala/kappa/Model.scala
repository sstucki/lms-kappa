package kappa

import scala.language.implicitConversions

trait Model extends Patterns with Mixtures with Rules with Perturbations
with Parser with Symbols with Embeddings {
  self: LanguageContext =>

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
