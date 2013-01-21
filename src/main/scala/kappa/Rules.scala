package kappa

trait Rules {
  this: LanguageContext with Patterns with Embeddings with Actions =>

  var rules: Vector[Rule] = Vector()

  // Rules
  class Rule(val action: Action, val rate: (Int*) => Double) {
    //lazy val ones : Stream[Int] = Stream.cons(1, ones);
    override def toString =
      action.lhs.toString + " -> " +
      action.rhs.toString + " :@ " +
      //rate(ones.take(lhs.arity):_*);
      rate(1);

    // Add every instance of this class to the rules vector
    rules = rules :+ this;
  }
}
