package kappa

import org.scalatest.FlatSpec

class TestPatterns extends KappaModel with FlatSpec {

  behavior of "Patterns"

  contactGraph = "A(c!{1}, b!{2}), B(a!{2}, c!{3}), C(b!{3}, a!{1})"

  it should "connect sites correctly" in {
    val p: Pattern = p"A(b!1), B(a!1)"
    assert(p(0).sites(1).neighbour.get.agent.state === p(1).state)
    assert(p(1).sites(0).neighbour.get.agent.state === p(0).state)
    assert(p(0).sites(1).neighbour.get.state === p(1).sites(0).state)
    assert(p(1).sites(0).neighbour.get.state === p(0).sites(1).state)
  }

  it should "get components right" in {
    val p: Pattern = p"A(c!1, b!2), B(a!2, c!3), C(b!3, a!1)"
    assert(p.components.length === 1)
  }

  it should "get wildcards right" in {
    val p1: Pattern = p"A(b!_)"
    assert(p1(0).sites(1).link === SiteGraph.Wildcard(None, None, None))
    val p2: Pattern = p"A(b!A.b._)"
    assert(p2(0).sites(1).link ===
      SiteGraph.Wildcard(Some(p1(0).state), Some(p1(0).sites(1).state), None))
  }
}

