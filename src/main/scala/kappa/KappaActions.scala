package kappa

import scala.language.implicitConversions

trait KappaActions extends Actions {
  this: KappaContext with Agents with Patterns with Mixtures
      with Embeddings with PartialEmbeddings with Rules =>

  /** Factory object for building Kappa actions.  */
  object KappaAction {

    /**
     * Construct a Kappa action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     *
     * @param lhs the left-hand side of this action.
     * @param rhs the right-hand side of this action.
     */
    def apply(lhs: Pattern, rhs: Pattern): Action = {

      // Compute the offsets of the first agents of each component of
      // the LHS in the agents array passed to an action application.
      val ceOffsets: Array[Int] =
        lhs.components.scanLeft(0) { (i, ce) => i + ce.length }

      @inline def lhsAgentOffset(a: Pattern.Agent) =
        ceOffsets(a.component.index) + a.index

      // Find the longest common prefix, i.e. longest prefix in the
      // sequences of agents making up the LHS and RHS, for which the
      // number of sites and the agent states match up.
      val zipped = (lhs zip rhs)
      val firstDiffIndex = zipped indexWhere {
        case (la, ra) =>
          !((la.indices == ra.indices) &&
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
      new Action(lhs, rhs, pe, rhsAgentOffsets, None, None)
    }
  }

  /** Convert a pair `(lhs, rhs)` of patterns into a Kappa action. */
  implicit def patternPairToKappaAction(lr: (Pattern, Pattern)): Action =
    KappaAction(lr._1, lr._2)
}

