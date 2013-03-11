package kappa

import org.scalatest.FlatSpec

class TestPatterns extends KappaModel with FlatSpec
{
  behavior of "Patterns"

  contactGraph = "A(c!{1}, b!{2}), B(a!{2}, c!{3}), C(b!{3}, a!{1})"

  it should "connect sites correctly" in {
    val p: Pattern = "A(b!1), B(a!1)"
    assert(p(0).sites(1).neighbour.get.agent.state == p(1).state)
    assert(p(1).sites(0).neighbour.get.agent.state == p(0).state)
    assert(p(0).sites(1).neighbour.get.state == p(1).sites(0).state)
    assert(p(1).sites(0).neighbour.get.state == p(0).sites(1).state)
  }

  it should "get components right" in {
    val p: Pattern = "A(c!1, b!2), B(a!2, c!3), C(b!3, a!1)"
    assert(p.components.length == 1)
  }
}

