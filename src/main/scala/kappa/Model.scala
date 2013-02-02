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
}

class KappaModel(val contactGraph: String) extends Model
  with KappaContext with KappaParser with KappaSymbols
{
  initSymbols(parseContactGraph(contactGraph) match {
    case Success(cg, _) => cg
    case msg => throw new IllegalArgumentException(
      "given contact graph is invalid: " + msg)
  })
}
