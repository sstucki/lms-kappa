package kappa

trait Perturbations {
  self: LanguageContext with Patterns =>

  var mods : Vector[Perturbation] = Vector()

  // Perturbations
  case class Cond(cond: self.type => Boolean) {
    // "do" would probably be a better name for this method, but it's not possible... exec perhaps?
    def set(effect: => Unit) = SideEffect(cond, () => effect)
    def add(n: Int) = Mod( Add(cond, n, _) )
    def del(n: Int) = Mod( Del(cond, n, _) )

    case class Mod(of: Pattern => Perturbation)
  }

  trait Perturbation {
    var repeatUntil : Option[self.type => Boolean] = None

    def until(cond: => Boolean) {
      repeatUntil = Some(_ => cond)
    }

    def until(cond: self.type => Boolean) {
      repeatUntil = Some(cond)
    }

    // Add every instance of this class to the mods vector
    self.mods = self.mods :+ this
  }

  case class SideEffect(cond: self.type => Boolean, effect: () => Unit) extends Perturbation
  case class Add(cond: self.type => Boolean, n: Int, m: Pattern) extends Perturbation
  case class Del(cond: self.type => Boolean, n: Int, m: Pattern) extends Perturbation
}

