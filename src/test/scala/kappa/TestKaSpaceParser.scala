package kappa

import org.scalatest.FlatSpec

class TestKaSpaceParser extends KaSpaceModel("A:{1}(s:{[2,0,0]})") with FlatSpec
{
  behavior of "KaSpace Parser"

  it should "parses states correctly" in {
    val radiusA = 1.0
    val xUnit = Vector(1.0, 0, 0)
    val sg1 = p"A : $radiusA (s : ${xUnit map (_ * 2)})"

    //assert(sg1 == List(AST.Agent("A", Some(radiusA), List(
    //  AST.Site("s", Some(xUnit map (_ * 2)), AST.Stub)))))

    assert(sg1(0).state == AgentStateImpl("A", Some(radiusA)))
    assert(sg1(0).sites(0).state ==
      SiteStateImpl("A", "s", Some(xUnit map (_ * 2))))
  }
}

