package kappa

import scala.collection.mutable

trait Rules {
  this: LanguageContext with Patterns with Embeddings with Actions =>

  var rules: Vector[Rule] = Vector()

  trait RuleBox {
    def unpack: Either[Rule, BiRule]
    def register: Unit
  }

  /** Register a rule in the model. */
  def registerRule(r: RuleBox) = r.register


  // Rules
  case class Rule(val action: Action, val law: () => Double)
      extends RuleBox {

    def unpack = Left(this)

    /** Register this rule in the model. */
    def register {

      // Register the components of LHS pattern
      action.lhs.registerComponents

      // Find positive influence of this rule on every registered
      // component and initialize the positive influence map
      // of the action accordingly.
      for (c <- patternComponents) yield {
        (c.modelIndex, action.addActivation(c))
      }

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

  case class BiRule(biaction: BiAction, fwdLaw: () => Double, bwdLaw: () => Double)
      extends RuleBox {

    def unpack = Right(this)

    /** Register this bidirectional rule in the model. */
    def register {

      // Register the components of LHS pattern
      biaction.lhs.registerComponents

      // Register the components of RHS pattern
      biaction.rhs.registerComponents

      val fwdAction = biaction.fwdAction
      val bwdAction = biaction.bwdAction

      // Find positive influence of this rule on every registered
      // component and initialize the positive influence map
      // of the action accordingly.
      for (c <- patternComponents) yield {
        fwdAction.addActivation(c)
        bwdAction.addActivation(c)
      }

      // Add this rule to the rules vector of the model
      rules = rules :+ (Rule(fwdAction, fwdLaw)) :+ (Rule(bwdAction, bwdLaw))

      println("# Registered rule # " + (rules.length - 1) + ": " + this)
    }

    override def toString =
      biaction.lhs.toString + " <-> " +
      biaction.rhs.toString + " :@ " +
      (fwdLaw(), bwdLaw())
  }
}
