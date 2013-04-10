package kappa

import scala.collection.mutable

trait Rules {
  this: LanguageContext
      with Patterns
      with Embeddings
      with Actions =>

  var rules: Vector[Rule] = Vector()

  trait RuleBox {
    def register: Unit
  }

  /** Register a rule in the model. */
  def registerRule(r: RuleBox) = r.register


  // -- Rules --

  case class Rule(val action: Action, val law: () => Double)
      extends RuleBox {

    /** Register this rule in the model. */
    def register {

      // Register the action
      action.register

      // Add this rule to the rules vector of the model
      rules = rules :+ this

      println("# Registered rule # " + (rules.length - 1) + ": " + this)
    }

    override def toString =
      action.lhs.toString + " -> " +
      action.rhs.toString + " :@ " +
      law()
  }


  // -- BiRules --

  case class BiRule(biaction: BiAction,
    fwdLaw: () => Double,
    bwdLaw: () => Double)
      extends RuleBox {

    val fwdAction = biaction.fwdAction
    val bwdAction = biaction.bwdAction

    /** Register this bidirectional rule in the model. */
    def register {
      Rule(fwdAction, fwdLaw).register
      Rule(bwdAction, bwdLaw).register
    }

    override def toString =
      biaction.lhs.toString + " <-> " +
      biaction.rhs.toString + " :@ " +
      (fwdLaw(), bwdLaw())
  }


  // TODO: Improve contact graphs first
  // def generateRulesFromContactGraph: Seq[Rule] = {
  // }
}
