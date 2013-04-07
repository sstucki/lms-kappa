package kappa

import scala.language.implicitConversions


trait KappaActions extends KappaLikeActions {
  this: KappaContext
      with SiteGraphs
      with Patterns
      with Mixtures
      with RollbackMachines
      with Embeddings
      with PartialEmbeddings
      with Rules =>

  /** Factory object for building Kappa actions.  */
  implicit object KappaActionBuilder extends ActionBuilder {

    type B = KappaLikeRuleBuilder

    /**
     * Construct a Kappa action from a LHS and RHS pattern
     * using the longest-common-prefix rule.
     *
     * @param lhs the left-hand side of this action.
     * @param rhs the right-hand side of this action.
     */
    def apply(lhs: Pattern, rhs: Pattern): B = {

      val (pe, rhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(lhs, rhs)

      // Build the action
      new KappaLikeRuleBuilder(new Action(lhs, rhs, pe,
        rhsAgentOffsets, None, None))
    }
  }

  /** Factory object for building bidirectional Kappa actions.  */
  implicit object KappaBiActionBuilder extends BiActionBuilder {

    type B = KappaLikeBiRuleBuilder

    def apply(lhs: Pattern, rhs: Pattern): B = {

      val (fwdPe, rhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(lhs, rhs)

      val (bwdPe, lhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(rhs, lhs)

      new KappaLikeBiRuleBuilder(new BiAction(lhs, rhs, fwdPe, bwdPe,
        lhsAgentOffsets, rhsAgentOffsets, None, None))
    }
  }
}

