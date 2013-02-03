package kappa

import scala.collection.mutable

trait Rules {
  this: LanguageContext with Patterns with Embeddings with Actions =>

  var rules: Vector[Rule] = Vector()

  // Rules
  class Rule(val action: Action, val law: () => Double) {

    // RHZ: should we have an inverse method to easily create reversible rules?

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
      rules = rules :+ this;

      println("# Registered rule # " + (rules.length - 1) + ": " + this)
    }

    //lazy val ones : Stream[Int] = Stream.cons(1, ones);
    override def toString =
      action.lhs.toString + " -> " +
      action.rhs.toString + " :@ " +
      //rate(ones.take(lhs.arity):_*)
      law()

    // Register every instance of this class in the model
    register
  }
}
