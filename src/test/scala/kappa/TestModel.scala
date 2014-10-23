package kappa

import org.scalatest.FlatSpec

class KaModel extends KappaModel("A(s:{p,q}!{1,1}") {
  println("Contact graph = " + contactGraph)
  //import HelperFns._

  // Manual setup of Symbol table
  //
  // FIXME: This is a hack! It's just here to make the test work. Once
  // parsing the contact graph of a model works, this should probably
  // be removed, and the various tables in Symbols should be made
  // immutable.  Unless, of course, we want to reserve the option to
  // add symbols to the symbol table manually...
  agentTypes    = agentTypes    :+ "A"       :+ "B"
  agentTypeSyms = agentTypeSyms + ("A" -> 0) + ("B" -> 1)
  siteNames     = siteNames     :+ "s"
  siteNameSyms  = siteNameSyms  + ("s" -> 0)
  siteStateNames     = siteStateNames    :+ "p"       :+ "q"
  siteStateNameSyms  = siteStateNameSyms + ("p" -> 0) + ("q" -> 1)
  linkStateNames     = linkStateNames    :+ "l"
  linkStateNameSyms  = linkStateNameSyms + ("l" -> 0)

  val a  = new AgentState(0)
  val b  = new AgentState(1)
  val s  = new SiteState(0, None)
  val sp = new SiteState(0, Some(0))
  val sq = new SiteState(0, Some(1))
  val l  = new LinkState(Some(0))

  import Pattern._

  // Rule 0
  val lhs0 =
    Pattern() :+ (a, Site(s, true)) :+ (a, Site(s, true))
  val rhs0 =
    (Pattern() :+ (a, Site(s)) :+ (a, Site(s))) connect (1, 0, l, 0, 0, l)
  val r0 = lhs0 -> rhs0 :@ 1
  println(r0.action.atoms)

  val r1 = (Pattern() :+ (a, Site(sp, Wildcard(Some(a), None, None)))) ->
    ((Pattern() :+ (a, Site(sq)) :+ (b, Site(sp)))
      connect (1, 0, l, 0, 0, l)) :@ 2
  println(r1.action.atoms)

  withObs(lhs0, "LHS r0")
  withObs(r1.action.lhs, "LHS r1")
  withObs(Pattern() :+ (b, Site(s)))
  withObs(Pattern() :+ (a, Site(s)))

  // var k = 5
  // val r1 = "A(s), A(s)" -> "A(s!1), A(s!1)" :@ k * 2
  // println(r1)
  // k = 6
  // println(r1)

  // val r2 = "A(s!1), A(s!1)" -> "A(s), A(s)" !@ { ccs => k += 1; ccs(0) * 10 } // is there a way to do this with :@

  // // Quasi-steady-state approximation
  // val vmax = 1
  // val km = 1
  // val r3 = "A(s:p)" -> "A(s:q)" !@ (ccs => vmax * ccs(0) / (km + ccs(0)))

  withInit(Mixture(Pattern() :+ (a, Site(sp, true))) * 100)
  println("mixture: " + mix)

  // val pes = {
  //   (for (c <- lhs0.components) yield (c partialEmbeddingsIn m)) ++
  //   (for (c <- rhs0.components) yield (c partialEmbeddingsIn m))
  // }.flatten
  // println("partial embeddings:")
  // for (pe <- pes) println("  " + pe)

  // val m1 = when ("A(s)".inMix < 10) set (k = 7)
  // val m2 = when (10 > "A(s)".inMix) set println("k = 7")

  // val m3 = when (_.time >= 10) add 50 of "A(s)"
  // // NB: since the perturbation condition is an opaque function, we will probably have
  // //     this problem: https://github.com/jkrivine/KaSim/issues/21

  // val m4 = when (_.events == 1000) del 50 of "A(s)"

  withMaxEvents(100)
}

class KaModelTest extends FlatSpec {
  val model = new KaModel
  model.run
}
