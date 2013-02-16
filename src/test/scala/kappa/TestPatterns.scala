package kappa

import org.scalatest.FlatSpec

class TestPatterns extends KappaModel with FlatSpec
{
  behavior of "Patterns"

  contactGraph = "A(s!{1}), B(s!{1})"

  it should "connect sites correctly" in {
    val p1 = Pattern("A(s!1), B(s!1)")
    assert(p1(0).sites(0).neighbour.get.agent.state == p1(1).state)
    assert(p1(1).sites(0).neighbour.get.agent.state == p1(0).state)
    assert(p1(0).sites(0).neighbour.get.state == p1(1).sites(0).state)
    assert(p1(1).sites(0).neighbour.get.state == p1(0).sites(0).state)
  }

  it should "get components right" in {
    val p1 = Pattern("A(x!1, y!2), B(x!2, y!3), C(x!3, y!1)")
    assert(p1.components.length == 1)
  }
}

