package kappa

import org.scalatest.FlatSpec

class TestPatterns extends KappaModel with FlatSpec
{
  behavior of "Patterns"

  contactGraph = "A(s!{1}), B(s!{1})"

  it should "connect sites correctly" in {
    val p1: Pattern = "A(s!1), B(s!1)"
    assert(p1(0).neighbour(0).get._1.state == p1(1).state)
    assert(p1(1).neighbour(0).get._1.state == p1(0).state)
    assert(p1(0).sites(0).neighbour.get.state == p1(1).sites(0).state)
    assert(p1(1).sites(0).neighbour.get.state == p1(0).sites(0).state)
  }
}

