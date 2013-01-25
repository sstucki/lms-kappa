package kappa

import scala.collection.mutable

trait Rules {
  this: LanguageContext with Patterns with Embeddings with Actions =>

  var rules: Vector[Rule] = Vector()

  // Rules
  class Rule(val action: Action, val rate: (Int*) => Double) {

    /** Register this rule in the model. */
    def register {

      // Register the components of RHS and LHS patterns
      action.lhs.registerComponents
      action.rhs.registerComponents

      // Find positive influence of this rule on every registered
      // component and initialize the positive influence map
      // of the action accordingly.
      for (c <- patternComponents) yield {
        (c.modelIndex, action.addPositiveInfluence(c))
      }

      // Add this rule to the rules vector of the model
      rules = rules :+ this;
    }

    //lazy val ones : Stream[Int] = Stream.cons(1, ones);
    override def toString =
      action.lhs.toString + " -> " +
      action.rhs.toString + " :@ " +
      //rate(ones.take(lhs.arity):_*);
      rate(1);

    // Register every instance of this class in the model
    register
  }
}
