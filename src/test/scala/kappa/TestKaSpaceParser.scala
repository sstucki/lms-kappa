package kappa

import org.scalatest.FlatSpec

class TestKaSpaceParser extends KaSpaceModel with FlatSpec
{
  behavior of "KaSpace Parser"

  contactGraph = "A:{1}(s:{[2,0,0]})"

  it should "parses states correctly" in {
    val radiusA = 1.0
    val xUnit = Position(1.0, 0, 0)
    val sg1 = p"A : $radiusA (s : ${xUnit * 2})"

    //assert(sg1 == List(AST.Agent("A", Some(radiusA), List(
    //  AST.Site("s", Some(xUnit map (_ * 2)), AST.Stub)))))

    assert(sg1(0).state == KaSpaceAgentState("A", Some(radiusA)))
    assert(sg1(0).sites(0).state ==
      KaSpaceSiteState("A", "s", Some(xUnit * 2)))
  }
}

