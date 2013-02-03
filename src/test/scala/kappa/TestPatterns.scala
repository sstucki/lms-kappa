package kappa

import org.scalatest.FlatSpec

class TestPatterns extends KappaModel("A(s!{1}), B(s!{1})") with FlatSpec
{
  behavior of "Patterns"

  it should "connect sites correctly" in {
    val p1 = Pattern("A(s!1), B(s!1)")
    assert(p1(0).sites(0).neighbour.get.agent.state == p1(1).state)
    assert(p1(1).sites(0).neighbour.get.agent.state == p1(0).state)
    assert(p1(0).sites(0).neighbour.get.state == p1(1).sites(0).state)
    assert(p1(1).sites(0).neighbour.get.state == p1(0).sites(0).state)
  }
}

