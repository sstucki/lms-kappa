package kappa

import scala.language.postfixOps

trait Rules {
  this: LanguageContext with Symbols with Patterns with Embeddings =>

  var rules: Vector[Rule] = Vector()

  // Actions
  // FIXME!
  class Action(lhs: Pattern, rhs: Pattern) extends Function1[Embedding, Unit] {
    def apply(embedding: Embedding) {}
    def :@ (rate: => Double) = new Rule(lhs, rhs, ns => ns.product * rate)
    def !@ (law: (Int*) => Double) = new Rule(lhs, rhs, law) // see https:/ / github.com/jkrivine/KaSim/issues/9
  }

  // Rules
  class Rule(lhs: Pattern, rhs: Pattern, rate: (Int*) => Double) {
    lazy val ones : Stream[Int] = Stream.cons(1, ones);
    override def toString() = lhs.toString + " -> " + rhs.toString + " :@ " + rate//rate(ones.take(lhs.arity):_*);

    // Add every instance of this class to the rules vector
    rules = rules :+ this;
  }
}
