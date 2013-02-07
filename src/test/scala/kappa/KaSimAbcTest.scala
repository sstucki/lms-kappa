package kappa

import scala.language.postfixOps

import org.scalatest.FlatSpec


class KaSimAbcTest extends KappaModel("") with FlatSpec {

  // ####### TEMPLATE MODEL AS DESCRIBED IN THE KASIM MANUAL #############

  // #### Signatures
  // %agent: A(x,c) # Declaration of agent A
  // %agent: B(x) # Declaration of B
  // %agent: C(x1~u~p,x2~u~p) # Declaration of C with 2 modifiable sites

  // Manual setup of Symbol table
  //
  // FIXME: This is a hack! It's just here to make the test work. Once
  // parsing the contact graph of a model works, this should probably
  // be removed, and the various tables in Symbols should be made
  // immutable.  Unless, of course, we want to reserve the option to
  // add symbols to the symbol table manually...
  agentTypes    = agentTypes    :+ "A"       :+ "B"       :+ "C"
  agentTypeSyms = agentTypeSyms + ("A" -> 0) + ("B" -> 1) + ("C" -> 2)
  siteNames     = siteNames     :+ "x"       :+ "c"       :+ "x1"       :+
                                   "x2"
  siteNameSyms  = siteNameSyms  + ("x" -> 0) + ("c" -> 1) + ("x1" -> 2) +
                                  ("x2" -> 3)
  siteStateNames     = siteStateNames    :+ "."       :+ "u"       :+ "p"
  siteStateNameSyms  = siteStateNameSyms + ("." -> 0) + ("u" -> 1) + ("p" -> 2)

  val a  = new AgentState(0)
  val b  = new AgentState(1)
  val c  = new AgentState(2)
  val sx  = new SiteState(0, None)
  val sxb  = new SiteState(0, Some(0))
  val sc  = new SiteState(1, None)
  val scb  = new SiteState(1, Some(0))
  val sx1  = new SiteState(2, None)
  val sx1u  = new SiteState(2, Some(1))
  val sx1p  = new SiteState(2, Some(2))
  val sx2  = new SiteState(3, None)
  val sx2u  = new SiteState(3, Some(1))
  val sx2p  = new SiteState(3, Some(2))
  val l = KappaSiteBuilder.defaultLinkState // FIXME: Hack!


  // #### Variables

  // %var: 'on_rate' 1.0E-4 # per molecule per second
  val on_rate = 1.0E-4 // per molecule per second

  // %var: 'off_rate' 0.1 # per second
  val off_rate = 0.1   // per second

  // %var: 'mod_rate' 1 # per second
  val mod_rate = 1     // per second


  import Pattern._

  // #### Rules

  // 'a.b' A(x),B(x) <-> A(x!1),B(x!1) @ 'on_rate','off_rate' #A binds B
  val r0f = a(sx, sc?) ~ b(sx) -> a(sx!1, sc?) ~ b(sx!1) :@ on_rate

  // #'a..b' A(x!1),B(x!1) -> A(x),B(x) @ 'off_rate' # AB dissociation
  val r0r = a(sx!1, sc?) ~ b(sx!1) -> a(sx, sc?) ~ b(sx) :@ off_rate

  // 'ab.c' A(x!_,c),C(x1~u) ->A(x!_,c!2),C(x1~u!2) @ 'on_rate' # AB binds C
  val r1 =
    a(sx!*, sc) ~ c(sx1u, sx2?) -> a(sx!*, sc!2) ~ c(sx1u!2, sx2?) :@ on_rate

  // 'mod x1' C(x1~u!1),A(c!1) ->C(x1~p),A(c) @ 'mod_rate' # AB modifies x1
  val r2 =
    c(sx1u!1, sx2?) ~ a(sx?, sc!1) -> c(sx1p, sx2?) ~ a(sx?, sc) :@ mod_rate

  // 'a.c' A(x,c),C(x1~p,x2~u) -> A(x,c!1),C(x1~p,x2~u!1) @ 'on_rate'
  // # A binds C on x2
  val r3 =
    a(sx, sc) ~ c(sx1p, sx2u) -> a(sx, sc!1) ~ c(sx1p, sx2u!1) :@ on_rate

  // 'mod x2' A(x,c!1),C(x1~p,x2~u!1) -> A(x,c),C(x1~p,x2~p) @ 'mod_rate'
  // # A modifies x2
  val r4 =
    a(sx, sc!1) ~ c(sx1p, sx2u!1) -> a(sx, sc) ~ c(sx1p, sx2p) :@ mod_rate


  // #### Variables (cont)
  // %obs: 'AB' A(x!x.B)
  withObs(a(sx!Wildcard(Some(b), Some(sx), None), sc?), "AB")
  // %obs: 'Cuu' C(x1~u?,x2~u?)
  withObs(c(sx1u?, sx2u?), "Cuu")
  // %obs: 'Cpu' C(x1~p?,x2~u?)
  withObs(c(sx1p?, sx2u?), "Cpu")
  // %obs: 'Cpp' C(x1~p?,x2~p?)
  withObs(c(sx1p?, sx2p?), "Cpp")

  // %var: 'n_a' 1000
  val n_a = 100
  // %obs: 'n_b' 'n_a'
  val n_b = n_a
  // %var: 'n_c' 10000
  val n_c = 10 * n_a


  // #### Initial conditions
  // %init: 'n_a' A()
  withInit(Mixture(Pattern() :+ (a, Site(sxb, true), Site(scb, true))) * n_a)
  // %init: 'n_b' B()
  withInit(Mixture(Pattern() :+ (b, Site(sxb, true))) * n_b)
  // %init: 'n_c' C()
  withInit(Mixture(Pattern() :+ (c, Site(sx1u, true), Site(sx2u, true))) * n_c)


  withMaxEvents(10000)
  withMaxTime(3000)
  run
}

