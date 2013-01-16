package kappa

import org.scalatest.FlatSpec

class TestModel extends KappaModel("A(s:{p,q}!{1,1}") with FlatSpec {

  println("Contact graph = " + contactGraph)
  //import HelperFns._

  // Manual setup of Symbol table
  agentTypes    = agentTypes    :+ "A"       :+ "B"
  agentTypeSyms = agentTypeSyms + ("A" -> 0) + ("B" -> 1)
  siteNames     = siteNames     :+ "s"
  siteNameSyms  = siteNameSyms  + ("s" -> 0)
  siteStateNames     = siteStateNames    :+ "p"       :+ "q"
  siteStateNameSyms  = siteStateNameSyms + ("p" -> 0) + ("q" -> 1)
  
  val a  = new AgentState(0)
  val b  = new AgentState(1)
  val s  = new SiteState(0, None)
  val sq = new SiteState(0, Some(1))
  val l  = new LinkState(None)

  import Pattern._

  // LHS
  val s1 = Site(s, true); val s2 = s1.copy()
  val a1 = Agent(a, Vector(s1)); val a2 = Agent(b, Vector(s2))
  val lhs = Pattern() :+ a1 :+ a2

  val s3 = Site(s); val s4 = Site(sq)
  val rhs = ((Pattern() :+ Agent(a, Vector(s3)) :+ Agent(a, Vector(s4)))
             connect (1, 0, l, 0, 0, l))

  val k = 5
  val r0 = lhs -> rhs :@ k * 2

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

  val m = Mixture(rhs) * 10
  println("mixture: " + m)

  // val m1 = when ("A(s)".inMix < 10) set (k = 7)
  // val m2 = when (10 > "A(s)".inMix) set println("k = 7")

  // val m3 = when (_.time >= 10) add 50 of "A(s)"
  // // NB: since the perturbation condition is an opaque function, we will probably have
  // //     this problem: https://github.com/jkrivine/KaSim/issues/21

// val m4 = when (_.events == 1000) del 50 of "A(s)"

println(rules)

  run
}

