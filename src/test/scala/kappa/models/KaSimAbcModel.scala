package kappa.models

import scala.language.postfixOps

import kappa.KappaModel


class KaSimAbcModel extends KappaModel {

  // ####### TEMPLATE MODEL AS DESCRIBED IN THE KASIM MANUAL #############

  // #### Signatures
  // %agent: A(x,c) # Declaration of agent A
  // %agent: B(x) # Declaration of B
  // %agent: C(x1~u~p,x2~u~p) # Declaration of C with 2 modifiable sites

  contactGraph = "A(x!{1},c!{2,3}),B(x!{1}),C(x1~{u,p}!{2},x2~{u,p}!{3})"


  // #### Rates

  // %var: 'on_rate' 1.0E-4 # per molecule per second
  val on_rate = 1.0E-4 // per molecule per second

  // %var: 'off_rate' 0.1 # per second
  val off_rate = 0.1   // per second

  // %var: 'mod_rate' 1 # per second
  val mod_rate = 1     // per second


  // #### Rules

  // 'a.b' A(x),B(x) <-> A(x!1),B(x!1) @ 'on_rate','off_rate' # A binds B
  val r0f = withRule { "A(x), B(x)" -> "A(x!1), B(x!1)" :@ on_rate }

  // #'a..b' A(x!1),B(x!1) -> A(x),B(x) @ 'off_rate' # AB dissociation
  val r0r = withRule { "A(x!1), B(x!1)" -> "A(x), B(x)" :@ off_rate }

  // 'ab.c' A(x!_,c),C(x1~u) -> A(x!_,c!2),C(x1~u!2) @ 'on_rate' # AB binds C
  val r1 = withRule { "A(x!_,c), C(x1~u)" -> "A(x!_,c!2), C(x1~u!2)" :@ on_rate }

  // 'mod x1' C(x1~u!1),A(c!1) -> C(x1~p),A(c) @ 'mod_rate' # AB modifies x1
  val r2 = withRule { "C(x1~u!1), A(c!1)" -> "C(x1~p), A(c)" :@ mod_rate }

  // 'a.c' A(x,c),C(x1~p,x2~u) -> A(x,c!1),C(x1~p,x2~u!1) @ 'on_rate' # A binds C on x2
  val r3 = withRule { "A(x,c), C(x1~p,x2~u)" -> "A(x,c!1), C(x1~p,x2~u!1)" :@ on_rate }

  // 'mod x2' A(x,c!1),C(x1~p,x2~u!1) -> A(x,c),C(x1~p,x2~p) @ 'mod_rate' # A modifies x2
  val r4 = withRule { "A(x,c!1), C(x1~p,x2~u!1)" -> "A(x,c), C(x1~p,x2~p)" :@ mod_rate }


  // #### Observables

  // %obs: 'AB' A(x!x.B)
  withObs("AB")("A(x!_)")

  // %obs: 'Cuu' C(x1~u?,x2~u?)
  withObs("Cuu")("C(x1~u?,x2~u?)")

  // %obs: 'Cpu' C(x1~p?,x2~u?)
  withObs("Cpu")("C(x1~p?,x2~u?)")

  // %obs: 'Cpp' C(x1~p?,x2~p?)
  withObs("Cpp")("C(x1~p?,x2~p?)")


  // #### Variables

  // %var: 'n_a' 1000
  val n_a = 1000

  // %obs: 'n_b' 'n_a'
  val n_b = n_a

  // %var: 'n_c' 10000
  val n_c = 10 * n_a


  // #### Initial conditions

  // %init: 'n_a' A()
  withInit(n_a)("A(x, c)")

  // %init: 'n_b' B()
  withInit(n_b)("B(x)")

  // %init: 'n_c' C()
  withInit(n_c)("C(x1~u,x2~u)")


  when (this.events == 999999) exec {
    var maxCc: Int = 0
    for (cc <- stringToPattern(this.mix.toString).components)
      if (maxCc < cc.length) maxCc = cc.length
    println(maxCc)
  }


  // #### Simulate!

  maxEvents = 1000000
  run
}

object KaSimAbcModelMain {
  def main(args: Array[String]): Unit = new KaSimAbcModel
}

