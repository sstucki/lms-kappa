package kappa

import org.scalatest.FlatSpec

class TestModel extends KappaModel with FlatSpec
{
  //behavior of "Models"

  contactGraph = "A(s:{p,q}!{1,1,2}),B(s!{2})"

  // Manual setup of Symbol table
  //
  // FIXME: This is a hack! It's just here to make the test work. Once
  // parsing the contact graph of a model works, this should probably
  // be removed, and the various tables in Symbols should be made
  // immutable.  Unless, of course, we want to reserve the option to
  // add symbols to the symbol table manually...
  //
  // RHZ: It's not possible with the current strategy to make the various
  // tables in Symbols immutable, because initSymbols has to make
  // assignments

  var k = 5
  val r1 = "A(s), A(s)" -> "A(s!1), A(s!1)" :@ k * 1E-4
  withRule(r1)
  println(r1.action.atoms)
  println(r1)
  k = 6
  println(r1)

  val r2 = withRule("A(s:p!A)" -> "A(s:q!1), B(s!1)" :@ 1)
  println(r2.action.atoms)

  withObs(r1.action.lhs, "LHS r1")
  withObs(r2.action.lhs, "LHS r2")
  withObs("B()", "B")
  withObs("A(s:q)", "Aq")

  val r3 = withRule("A(s!1), A(s!1)" -> "A(s), A(s)" :@ 1)

  // Quasi-steady-state approximation
  val vmax = 1
  val kM = 1
  val r4 = withRule("A(s:p)" -> "A(s:q)" :@ (vmax / (kM + "A(s:p)".inMix)))

  withInit(Mixture("A(s:p)") * 100)
  println("Mixture: " + mix)

  val m1 = when ("A(s)".inMix < 10) set (k = 7)
  val m2 = when (10 > "A(s)".inMix) set println("k = 7")

  val m3 = when (_.time >= 10) add 50 of "A(s)"
  // RHZ: since the perturbation condition is an opaque function, we will probably
  //      have this problem: https://github.com/jkrivine/KaSim/issues/21

  val m4 = when (_.events == 1000) del 50 of "A(s)"

  withMaxEvents(100)
  run
}
