package kappa

import scala.collection.mutable

import scala.language.implicitConversions

/** A class representing Kappa models. */
class KappaModel extends Model with KappaContext with KappaActions
    with KappaAbstractSyntax with KappaParser

