package kappa

import org.scalatest.FlatSpec

class TestKaSpaceParser extends KaSpaceModel with FlatSpec
{
  behavior of "KaSpace parser"

  contactGraph = "A:{1}(s:{[2,0,0]})"

  it should "parse states correctly" in {
    val radiusA = 1.0
    val xUnit = Position(1, 0, 0)
    val p = p"A : $radiusA (s : ${xUnit * 2})"

    val agentStateSet = contactGraph.agentStateSets(0)

    assert(contactGraph.agentStateSets(0) ===
      KaSpaceAgentStateSet("A", List(radiusA)))
    assert(contactGraph.siteStateSets(0) ===
      KaSpaceSiteStateSet("s", List(xUnit * 2), agentStateSet))

    assert(p(0).state === KaSpaceAgentState(agentStateSet, Some(radiusA)))
    assert(p(0).sites(0).state === KaSpaceSiteState(siteStateSet, Some(xUnit * 2)))

    assert(p(0).state.label === Some(radiusA))
    assert(p(0).sites(0).state.label === Some(xUnit * 2))
  }
}

