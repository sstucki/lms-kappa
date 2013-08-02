package kappa

import scala.language.implicitConversions


trait TKappaActions extends KappaLikeActions {
  this: KappaContext
      with SiteGraphs
      with Patterns
      with Mixtures
      with Embeddings
      with PartialEmbeddings
      with Rules
      with TKappaModel =>

  /** Factory object for building KaSpace actions.  */
  implicit object TKappaActionBuilder extends ActionBuilder {

    type RuleBuilder = KappaLikeRuleBuilder

    /**
     * Construct a KaSpace action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     *
     * @param lhs the left-hand side of this action.
     * @param rhs the right-hand side of this action.
     */
    def apply(lhs: Pattern, rhs: Pattern): RuleBuilder = {

      val (pe, rhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(lhs, rhs)

      // Build the action
      new KappaLikeRuleBuilder(new Action(lhs, rhs, pe,
        rhsAgentOffsets, None, None))
    }
  }


  /** Factory object for building bidirectional KaSpace actions.  */
  implicit object TKappaBiActionBuilder extends BiActionBuilder {

    type BiRuleBuilder = TKappaBiRuleBuilder

    def apply(lhs: Pattern, rhs: Pattern): BiRuleBuilder = {

      val (fwdPe, rhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(lhs, rhs)

      val (bwdPe, lhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(rhs, lhs)

      new TKappaBiRuleBuilder(new BiAction(lhs, rhs, fwdPe, bwdPe,
        lhsAgentOffsets, rhsAgentOffsets,
        Some(Thermodynamics.updateEnergy),
        Some(Thermodynamics.checkEnergy)))
    }
  }


  final class TKappaBiRuleBuilder(biaction: BiAction)
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
    def :@(rate: => Double) = withRateLaws(
      mkRateLawFromRate(rate, biaction.lhs),
      mkRateLawFromRate(rate, biaction.rhs))

    // RHZ: Proposal to solve the operator precedence issue (letters
    // have the lowest precedence which is exactly what we want)
    def at(rate: => Double) = :@(rate)

    /**
     * Constructor for rules that follow an arbitrary rate law.
     *
     * @param fwdLaw kinetic law of forward rule
     * @param bwdLaw kinetic law of backward rule
     */
    def !@(law: => Double) = withRateLaws(() => law, () => law)

    // But we would have to find a better name for this one: at! doesn't work
    // def at!(law: => Double) = !@(law)
  }


  object Thermodynamics {

    var lastEnergy: Double = 0

    /**
     * Check whether we accept or not the transition given the
     * difference in energy between the source and target state.
     */
    def checkEnergy(action: Action, agents: Action.Agents): Boolean = {
      val nextEnergy = mixtureEnergy
      lazy val deltaE = nextEnergy - lastEnergy
      lazy val prob = math.exp(-deltaE / kB * temperature)
      (lastEnergy >= nextEnergy) || (rand.nextDouble < prob)
    }

    def updateEnergy(action: Action, agents: Action.Agents): Boolean = {
      lastEnergy = mixtureEnergy
      true
    }
  }
}

