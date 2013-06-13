package kappa

trait KappaLikeActions extends Actions {
  this: KappaLikeContext
      with SiteGraphs
      with Patterns
      with Mixtures
      with RollbackMachines
      with Embeddings
      with PartialEmbeddings
      with Rules =>

  object KappaLikeActionBuilder {

    def commonLongestPrefix(lhs: Pattern, rhs: Pattern):
        (PartialEmbedding, Map[Pattern.Agent, AgentIndex]) = {

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

      (pe, rhsAgentOffsets)
    }
  }

  // NOTE: This prevents the multiplication of lhs.inMix by
  // PositiveInfinity to produce NaN in most cases. However, if rate
  // evaluates to PositiveInfinity when lhs.inMix is 0 but not at the
  // onset of the simulation we will have the problem again.
  @inline final protected
  def mkRateLawFromRate(rate: => Double, lhs: Pattern) =
    if (rate == Double.PositiveInfinity) {
      () => if (lhs.inMix == 0) 0
            else lhs.inMix * rate
    } else {
      () => lhs.inMix * rate
    }

  final class KappaLikeRuleBuilder(action: Action)
      extends RuleBuilderIntf {

    def getAction: Action = action

    /**
     * Constructor for rules that follow an arbitrary rate law.
     *
     * @param law kinetic law
     */
    def withRateLaw(law: () => Double): Rule = Rule(action, law)

    /**
     * Constructor for rules that follow mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant
     */
    def :@(rate: => Double) =
      withRateLaw(mkRateLawFromRate(rate, action.lhs))

    /**
     * Constructor for rules that follow an arbitrary rate law.
     *
     * @param law kinetic law
     */
    def !@(law: => Double) = withRateLaw(() => law)
  }


  final class KappaLikeBiRuleBuilder(biaction: BiAction)
      extends BiRuleBuilderIntf {

    def getBiAction: BiAction = biaction

    /**
     * Constructor for rules that follow arbitrary rate laws.
     *
     * @param fwdLaw kinetic law of forward rule
     * @param bwdLaw kinetic law of backward rule
     */
    def withRateLaws(fwdLaw: () => Double, bwdLaw: () => Double) =
      BiRule(biaction, fwdLaw, bwdLaw)

    /**
     * Constructor for rules that follow mass-action kinetics.
     *
     * @param rate stochastic kinetic rate constant
     * @param fwdLaw kinetic law of forward rule
     * @param bwdLaw kinetic law of backward rule
     */
    def :@(fwdRate: => Double, bwdRate: => Double) = withRateLaws(
      mkRateLawFromRate(fwdRate, biaction.lhs),
      mkRateLawFromRate(bwdRate, biaction.rhs))

    // RHZ: Proposal to solve the operator precedence issue (letters
    // have the lowest precedence which is exactly what we want)
    def at(fwdRate: => Double, bwdRate: => Double) = :@(fwdRate, bwdRate)

    /**
     * Constructor for rules that follow an arbitrary rate law.
     *
     * @param fwdLaw kinetic law of forward rule
     * @param bwdLaw kinetic law of backward rule
     */
    def !@(fwdLaw: => Double, bwdLaw: => Double) =
      withRateLaws(() => fwdLaw, () => bwdLaw)
  }
}

