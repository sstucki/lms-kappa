package kappa

import scala.language.postfixOps

import org.scalatest.FlatSpec

class TestModel extends KappaModel with FlatSpec {

  //behavior of "Models"

  contactGraph = "A(s:{p,q}!{1,1,2}),B(s!{2})"

  var k = 5
  val r1 = ("A(s), A(s)" <-> "A(s!1), A(s!1)") :@ (k * 1E-4, 1)
  withRule(r1)
  println(r1)
  k = 6
  println(r1)

  val r2 = "A(s:p!A)" -> "A(s:q!1), B(s!1)" :@ 1
  withRule(r2)
  println(r2.action.atoms)

  obs (r1.biaction.lhs) named "LHS r1"
  obs (r2.action.lhs) named "LHS r2"
  obs ("B()") named "B"
  obs ("A(s:q)") named "Aq"

  // Quasi-steady-state approximation
  val vmax = 1
  val kM = 1
  val r3 = withRule("A(s:p)" -> "A(s:q)" :@ (vmax / (kM + "A(s:p)".inMix)))

  init (10 of "A(s:p)")
  println("Mixture: " + mix)

  when ("A(s)".inMix < 50) exec (k = 7)
  when (10 > "A(s)".inMix) exec { println("k = 8"); k = 8; }
  when (this.time >= 10) add 50 of "A(s:p)"
  when (this.events == 1000) del 50 of "A(s:p)"
  every (35.3 timeUnits) add 30 of "A(s:p)"

  maxEvents = 5
  run
}
