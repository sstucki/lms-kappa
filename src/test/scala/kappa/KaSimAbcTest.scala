package kappa

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
  linkStateNames     = linkStateNames    :+ "."
  linkStateNameSyms  = linkStateNameSyms + ("." -> 0)

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
  val l  = new LinkState(Some(0))


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
  val r0f = (Pattern() :+ (a, Site(sx, true), Site(sc)) :+
    (b, Site(sx, true))) -> (
    (Pattern() :+ (a, Site(sx), Site(sc)) :+ (b, Site(sx))) connect
      (1, 0, l, 0, 0, l)) :@ on_rate

  // #'a..b' A(x!1),B(x!1) -> A(x),B(x) @ 'off_rate' #AB dissociation
  val r0r = ((Pattern() :+ (a, Site(sx), Site(sc)) :+ (b, Site(sx)))
    connect (1, 0, l, 0, 0, l)) -> (
    Pattern() :+ (a, Site(sx, true), Site(sc)) :+
      (b, Site(sx, true))) :@ off_rate

  // 'ab.c' A(x!_,c),C(x1~u) ->A(x!_,c!2),C(x1~u!2) @ 'on_rate' #AB binds C
  val r1 = (Pattern() :+ (a, Site(sx, Wildcard(None, None, None)),
    Site(sc, true)) :+ (c, Site(sx1u, true), Site(sx2))) -> (
    (Pattern() :+ (a, Site(sx, Wildcard(None, None, None)), Site(sc)) :+
      (c, Site(sx1u), Site(sx2))) connect (0, 1, l, 1, 0, l)) :@ on_rate

  // 'mod x1' C(x1~u!1),A(c!1) ->C(x1~p),A(c) @ 'mod_rate' #AB modifies x1
  val r2 = ((Pattern() :+ (c, Site(sx1u), Site(sx2))) :+
    (a, Site(sx), Site(sc)) connect (0, 0, l, 1, 1, l)) -> (
    Pattern() :+ (c, Site(sx1p, true), Site(sx2)) :+
      (a, Site(sx), Site(sc, true))) :@ mod_rate

  // 'a.c' A(x,c),C(x1~p,x2~u) -> A(x,c!1),C(x1~p,x2~u!1) @ 'on_rate' #A binds C on x2
  val r3 = (Pattern() :+ (a, Site(sx, true), Site(sc, true)) :+
    (c, Site(sx1p, true), Site(sx2u, true))) -> (
    (Pattern() :+ (a, Site(sx, true), Site(sc)) :+
      (c, Site(sx1p, true), Site(sx2u))) connect (0, 1, l, 1, 1, l)) :@ on_rate

  // 'mod x2' A(x,c!1),C(x1~p,x2~u!1) -> A(x,c),C(x1~p,x2~p) @ 'mod_rate' #A modifies x2
  val r4 = ((Pattern() :+ (a, Site(sx, true), Site(sc)) :+
    (c, Site(sx1p, true), Site(sx2u))) connect (0, 1, l, 1, 1, l)) -> (
    Pattern() :+ (a, Site(sx, true), Site(sc, true)) :+
      (c, Site(sx1p, true), Site(sx2p, true))) :@ mod_rate


  // #### Variables (cont)
  // %obs: 'AB' A(x!x.B)
  withObs(Pattern() :+ (a, Site(sx, Wildcard(Some(b), Some(sx), None)),
    Site(sc)))
  // %obs: 'Cuu' C(x1~u?,x2~u?)
  withObs(Pattern() :+ (c, Site(sx1u), Site(sx2u)))
  // %obs: 'Cpu' C(x1~p?,x2~u?)
  withObs(Pattern() :+ (c, Site(sx1p), Site(sx2u)))
  // %obs: 'Cpp' C(x1~p?,x2~p?)
  withObs(Pattern() :+ (c, Site(sx1p), Site(sx2p)))

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
  withMaxTime(200)
  run
}

