package kappa

import scala.language.implicitConversions

trait KaSpaceActions extends Actions {
  this: KaSpaceContext with Patterns with Mixtures with Embeddings
      with PartialEmbeddings with Rules =>

  /** Factory object for building KaSpace actions.  */
  object KaSpaceAction {

    /**
     * Construct a KaSpace action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     *
     * @param lhs the left-hand side of this action.
     * @param rhs the right-hand side of this action.
     */
    def apply(lhs: Pattern, rhs: Pattern): Action = {

      import Pattern._

      // Compute the offsets of the first agents of each component of
      // the LHS in the agents array passed to an action application.
      val ceOffsets: Vector[Int] =
        lhs.components.scanLeft(0) { (i, ce) => i + ce.length }

      @inline def lhsAgentOffset(a: Agent) =
        ceOffsets(a.component.index) + a.index

      // Find the longest common prefix, i.e. longest prefix in the
      // sequences of agents making up the LHS and RHS, for which the
      // number of sites and the agent states match up.
      val zipped = (lhs zip rhs)
      val firstDiffIndex = zipped indexWhere {
        case (la, ra) =>
          !((la.length == ra.length) &&
            (la.state matchesInLongestCommonPrefix ra.state))
      }
      val commonPrefixLength =
        if (firstDiffIndex > 0) firstDiffIndex else zipped.length
      val longestCommonPrefix = zipped take commonPrefixLength

      // Construct the partial embedding corresponding to this action
      val pe = PartialEmbedding(
        lhs take commonPrefixLength, rhs take commonPrefixLength)

      // Compute the RHS agent offsets
      val rhsPrefixAgentOffsets =
        for (i <- 0 until commonPrefixLength)
        yield (rhs(i), lhsAgentOffset(lhs(i)))
      val rhsSuffixAgentOffsets =
        for (i <- commonPrefixLength until rhs.length)
        yield (rhs(i), lhs.length + i - commonPrefixLength)
      val rhsAgentOffsets =
        (rhsPrefixAgentOffsets ++ rhsSuffixAgentOffsets).toMap

      // Build the action
      new Action(lhs, rhs, pe, rhsAgentOffsets, None, Some(geometricConsistencyCheck))
    }
  }

  /** Convert a pair `(lhs, rhs)` of patterns into a KaSpace action. */
  implicit def patternPairToKaSpaceAction(lr: (Pattern, Pattern)): Action =
    KaSpaceAction(lr._1, lr._2)

  def geometricConsistencyCheck(mix: Action.Agents): Boolean = true
}

