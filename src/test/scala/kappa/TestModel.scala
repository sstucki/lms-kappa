package kappa

import org.scalatest.FlatSpec

class TestModel extends KappaModel("A(s:{p,q}!{1,1})") with FlatSpec {

  println("Contact graph = " + contactGraph)

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

  import Pattern._

  // val lhs = "A(s), B(s), A()"
  val lhs = "A(s), A(s)"
  val rhs = "A(s!1), A(s:q!1)"
  // val r0 = A(s), B(s), A() -> A(s!1), A(s:q!1) :@ k * 2

  var k = 5
  val r1 = "A(s), A(s)" -> "A(s!1), A(s!1)" :@ k * 2
  println(r1)
  k = 6
  println(r1)

  val r2 = "A(s!1), A(s!1)" -> "A(s), A(s)" !@ { ccs => k += 1; ccs(0) * 10 } // is there a way to do this with :@

  // Quasi-steady-state approximation
  val vmax = 1
  val kM = 1
  val r3 = "A(s:p)" -> "A(s:q)" !@ (ccs => vmax * ccs(0) / (kM + ccs(0)))

  val m = Mixture("A(s!1), A(s:q!1)") * 5
  println("mixture: " + m)

  val pes = {
    (for (c <- lhs.components) yield (c partialEmbeddingsIn m)) ++
    (for (c <- rhs.components) yield (c partialEmbeddingsIn m))
  }.flatten

  println("partial embeddings:")
  for (pe <- pes) println("  " + pe)

  val m1 = when ("A(s)".inMix < 10) set (k = 7)
  val m2 = when (10 > "A(s)".inMix) set println("k = 7")

  val m3 = when (_.time >= 10) add 50 of "A(s)"
  // NB: since the perturbation condition is an opaque function, we will probably have
  //     this problem: https://github.com/jkrivine/KaSim/issues/21

  val m4 = when (_.events == 1000) del 50 of "A(s)"

  println("rules:")
  for (r <- rules) println("  " + r)

  run
}
