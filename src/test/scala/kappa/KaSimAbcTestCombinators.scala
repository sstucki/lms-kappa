package kappa

import scala.language.postfixOps

import org.scalatest.FlatSpec

class KaSimAbcTestCombinators extends KappaModel with FlatSpec {

  // ####### TEMPLATE MODEL AS DESCRIBED IN THE KASIM MANUAL #############

  // #### Signatures
  // %agent: A(x,c) # Declaration of agent A
  // %agent: B(x) # Declaration of B
  // %agent: C(x1~u~p,x2~u~p) # Declaration of C with 2 modifiable sites

  contactGraph = "A(x!{1},c!{2,3}),B(x!{1}),C(x1:{u,p}!{2},x2:{u,p}!{3})"

  object A extends AgentType("A")
  object B extends AgentType("B")
  object C extends AgentType("C")
  val x  = Site("x")
  val c  = Site("c")
  val x1 = Site("x1")
  val x2 = Site("x2")
  val u  = "u"
  val p  = "p"


  // #### Rates

  // %var: 'on_rate' 1.0E-4 # per molecule per second
  val on_rate = 1.0E-4 // per molecule per second

  // %var: 'off_rate' 0.1 # per second
  val off_rate = 0.1   // per second

  // %var: 'mod_rate' 1 # per second
  val mod_rate = 1     // per second


  import Pattern._

  // #### Rules

  // 'a.b' A(x),B(x) <-> A(x!1),B(x!1) @ 'on_rate','off_rate' #A binds B
  val r0f = (A(x) :: B(x)) -> (A(x!1) :: B(x!1)) :@ on_rate
  withRule(r0f)

  // #'a..b' A(x!1),B(x!1) -> A(x),B(x) @ 'off_rate' # AB dissociation
  val r0r = withRule(A(x!1) :+ B(x!1) -> A(x) :+ B(x) :@ off_rate)

  // 'ab.c' A(x!_,c),C(x1~u) ->A(x!_,c!2),C(x1~u!2) @ 'on_rate' # AB binds C
  val r1 = withRule(
    A(x!*, c), C(x1~u) -> A(x!*, c!2), C(x1~u!2) :@ on_rate
  )

  val rs = withRules(

    // 'mod x1' C(x1~u!1),A(c!1) ->C(x1~p),A(c) @ 'mod_rate' # AB modifies x1
    C(x1~u!1), A(c!1) -> C(x1~p), A(c) :@ mod_rate,

    // 'a.c' A(x,c),C(x1~p,x2~u) -> A(x,c!1),C(x1~p,x2~u!1) @ 'on_rate'
    // # A binds C on x2
    A(x, c), C(x1~p, x2~u) -> A(x, c!1), C(x1~p, x2~u!1) :@ on_rate,

    // 'mod x2' A(x,c!1),C(x1~p,x2~u!1) -> A(x,c),C(x1~p,x2~p) @ 'mod_rate'
    // # A modifies x2
    A(x, c!1), C(x1~p, x2~u!1) -> A(x, c), C(x1~p, x2~p) :@ mod_rate

  )
  val r2 = rs(0)
  val r3 = rs(1)
  val r4 = rs(2)

  // #### Variables (cont)
  // %obs: 'AB' A(x!x.B)
  withObs(A(x!(Some(B), Some(x), None)), "AB")
  // %obs: 'Cuu' C(x1~u?,x2~u?)
  withObs(C(x1~u?, x2~u?), "Cuu")
  // %obs: 'Cpu' C(x1~p?,x2~u?)
  withObs(C(x1~p?, x2~u?), "Cpu")
  // %obs: 'Cpp' C(x1~p?,x2~p?)
  withObs(C(x1~p?, x2~p?), "Cpp")

  // %var: 'n_a' 1000
  val n_a = 100
  // %obs: 'n_b' 'n_a'
  val n_b = n_a
  // %var: 'n_c' 10000
  val n_c = 10 * n_a


  // #### Initial conditions
  // %init: 'n_a' A()
  withInit(A(x, c), n_a)
  // %init: 'n_b' B()
  withInit(B(x), n_b)
  // %init: 'n_c' C()
  withInit(C(x1~u, x2~u), n_c)


  withMaxEvents(10000)
  withMaxTime(3000)
  run
}

