package kappa

/** A class representing Kappa models. */
abstract class KappaModel extends Model
    with KappaContext
    with KappaActions
    with KappaAbstractSyntax
    with KappaParsers

