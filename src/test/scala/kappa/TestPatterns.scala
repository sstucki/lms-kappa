package kappa

import org.scalatest.FlatSpec

class TestPatterns extends KappaModel with FlatSpec
{
  behavior of "Patterns"

  contactGraph = "A(c!{1}, b!{2}), B(a!{2}, c!{3}), C(b!{3}, a!{1})"

  it should "connect sites correctly" in {
    val p: Pattern = "A(b!1), B(a!1)"
    val nb1 = p(0).neighbour(1).get
    val nb2 = p(1).neighbour(0).get
    assert(nb1._1.state == p(1).state)
    assert(nb2._1.state == p(0).state)
    assert(nb1._1.sites(nb1._2).state == p(1).sites(0).state)
    assert(nb2._1.sites(nb2._2).state == p(0).sites(1).state)
  }

  it should "get components right" in {
    val p: Pattern = "A(c!1, b!2), B(a!2, c!3), C(b!3, a!1)"
    assert(p.components.length == 1)
  }
}

