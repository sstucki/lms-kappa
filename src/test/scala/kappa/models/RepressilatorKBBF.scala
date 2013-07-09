package kappa.models

import kappa.BioBrickFramework

// Edinburgh 2010 iGEM Team
//
// Core repressilator model - last update 29-03-2011
//
// This model has active core repressilator only.
//
// Observations are levels of LacI, TetR and cI.
//
// The core repressilator model was developed initially by Ty Thomson in 2009.
// The original version is available at:
// http://www.rulebase.org/showcase_books/182350-Rule-Based-Modeling-of-BioBrick-Parts
//
// DNA types: Within the model, the following DNA() types represent the various BioBrick sequences used
//
// BBaB0011 transcription terminator
// BBaB0034 ribosome binding site
// BBaC0012 lacI coding sequence
// BBaC0040 tetR coding sequence
// BBaC0051 lambda-cI coding sequence
// BBaR0010 lacI promoter
// BBaR0040 tetR promoter
// BBaR0051 lambda-cI promoter

class RepressilatorKBBFModel extends BioBrickFramework {

  // BBaR are promoters
  // BBaB are RBSes and terminators
  // BBaC are coding sequences
  contactGraph = """
    DNA(downstream!{1}, upstream!{1}, binding!{3,4,5,6},
        type~{BBaR0010p1,BBaR0010p2,BBaR0010p3,BBaR0010p4,
              BBaR0040p1,BBaR0040p2,BBaR0040p3,BBaR0040p4,
              BBaR0051p1,BBaR0051p2,BBaR0051p3,BBaR0051p4,
              BBaB0011,BBaB0034,BBaC0012,BBaC0040,BBaC0051}),
    RNA(downstream!{2},upstream!{2},binding!{7,8},
        type~{BBaB0034,BBaC0012,BBaC0040,BBaC0051,BBaR0010,BBaR0040,BBaR0051}),
    RNAP(dna!{3}, rna!{7}),
    Ribosome(rna!{8}),
    TetR(dna!{4}, atc),
    cI  (dna!{5}),
    LacI(dna!{6}, lactose)
  """


  // --- Parts and Devices ---

  withParts(
    new Promoter("BBaR0051") {
      bindingSites = 3
    },
    new Promoter("BBaR0010") {
      bindingSites = 3
    },
    new Promoter("BBaR0040") {
      bindingSites = 3
    },
    new RBS("BBaB0034"),
    new CodingSequence("BBaC0012") {
      protein = "LacI(dna,lactose)" // represses R10 (TetR)
    },
    new CodingSequence("BBaC0040") {
      protein = "TetR(dna,atc)" // represses R40 (cI)
    },
    new CodingSequence("BBaC0051") {
      protein = "cI(dna)" // represses R51 (LacI)
    },
    new Terminator("BBaB0011")
  )

  println("Parts:")
  for ((id, part) <- parts)
    println("  " + id + " -> " + part)

  withDevices(

    // LacI
    new Device { // R51 - C12
      promoter = "BBaR0051"
      rbs = "BBaB0034"
      codingSequence = "BBaC0012"
      terminator = "BBaB0011"
    },

    // TetR
    new Device { // R10 - C40
      promoter = "BBaR0010"
      rbs = "BBaB0034"
      codingSequence = "BBaC0040"
      terminator = "BBaB0011"
    },

    // cI
    new Device { // R40 - C51
      promoter = "BBaR0040"
      rbs = "BBaB0034"
      codingSequence = "BBaC0051"
      terminator = "BBaB0011"
    }
  )


  // --- Custom rules ---

  withRules(

    // -- Transcription factor binding to promoter region --

    // LacI binding to R0010p2 (no LacI){77308}
    ("DNA(binding,type~BBaR0010p3,upstream!2), LacI(dna,lactose), DNA(downstream!2,binding,type~BBaR0010p2)" <->
     "DNA(binding,type~BBaR0010p3,upstream!3), LacI(dna!1,lactose), DNA(downstream!3,binding!1,type~BBaR0010p2)") :@ (0.00996323269897635,2.24),

    // LacI binding to R0010p2 (LacI bound){77310}
    ("DNA(binding!1,type~BBaR0010p3,upstream!2), LacI(dna!1), DNA(downstream!2,binding,type~BBaR0010p2), LacI(dna,lactose)" <->
     "DNA(binding!2,type~BBaR0010p3,upstream!3), LacI(dna!2), DNA(downstream!3,binding!1,type~BBaR0010p2), LacI(dna!1,lactose)") :@
      (0.00996323269897635,0.09),

    // LacI binding to R0010p3 (no LacI){77309}
    ("DNA(binding,type~BBaR0010p3,upstream!2), LacI(dna,lactose), DNA(downstream!2,binding,type~BBaR0010p2)" <->
     "DNA(binding!1,type~BBaR0010p3,upstream!3), LacI(dna!1,lactose), DNA(downstream!3,binding,type~BBaR0010p2)") :@ (0.00996323269897635,2.24),

    // LacI binding to R0010p3 (LacI bound){77311}
    ("DNA(binding,type~BBaR0010p3,upstream!2), LacI(dna!1), DNA(downstream!2,binding!1,type~BBaR0010p2), LacI(dna,lactose)" <->
     "DNA(binding!1,type~BBaR0010p3,upstream!3), LacI(dna!2), DNA(downstream!3,binding!2,type~BBaR0010p2), LacI(dna!1,lactose)") :@
      (0.00996323269897635,0.09),

    // TetR binding to R0040p2 (no TetR){77296}
    ("DNA(binding,type~BBaR0040p3,upstream!2), TetR(dna,atc), DNA(downstream!2,binding,type~BBaR0040p2)" <->
     "DNA(binding,type~BBaR0040p3,upstream!3), TetR(dna!1,atc), DNA(downstream!3,binding!1,type~BBaR0040p2)") :@ (0.00996323269897635,2.24),

    // TetR binding to R0040p2 (TetR bound){77299}
    ("DNA(binding!1,type~BBaR0040p3,upstream!2), TetR(dna!1), TetR(dna,atc), DNA(downstream!2,binding,type~BBaR0040p2)" <->
     "DNA(binding!2,type~BBaR0040p3,upstream!3), TetR(dna!2), TetR(dna!1,atc), DNA(downstream!3,binding!1,type~BBaR0040p2)") :@ (0.00996323269897635,0.09),

    // TetR binding to R0040p3 (no TetR){77297}
    ("DNA(binding,type~BBaR0040p3,upstream!2), TetR(dna,atc), DNA(downstream!2,binding,type~BBaR0040p2)" <->
     "DNA(binding!1,type~BBaR0040p3,upstream!3), TetR(dna!1,atc), DNA(downstream!3,binding,type~BBaR0040p2)") :@ (0.00996323269897635,2.24),

    // TetR binding to R0040p3 (TetR bound){77300}
    ("DNA(binding,type~BBaR0040p3,upstream!2), TetR(dna,atc), TetR(dna!1), DNA(downstream!2,binding!1,type~BBaR0040p2)" <->
     "DNA(binding!2,type~BBaR0040p3,upstream!3), TetR(dna!2,atc), TetR(dna!1), DNA(downstream!3,binding!1,type~BBaR0040p2)") :@ (0.00996323269897635,0.09),

    // cI binding to R0051p2 (no cI){77258}
    ("DNA(binding,type~BBaR0051p3,upstream!2), cI(dna), DNA(downstream!2,binding,type~BBaR0051p2)" <->
     "DNA(binding,type~BBaR0051p3,upstream!3), cI(dna!1), DNA(downstream!3,binding!1,type~BBaR0051p2)") :@ (0.00996323269897635,2.24),

    // cI binding to R0051p2 (cI bound){77260}
    ("DNA(binding!1,type~BBaR0051p3,upstream!2), cI(dna), cI(dna!1), DNA(downstream!2,binding,type~BBaR0051p2)" <->
     "DNA(binding!2,type~BBaR0051p3,upstream!3), cI(dna!1), cI(dna!2), DNA(downstream!3,binding!1,type~BBaR0051p2)") :@ (0.00996323269897635,0.09),

    // cI binding to R0051p3 (no cI){77259}
    ("DNA(binding,type~BBaR0051p3,upstream!2), cI(dna), DNA(downstream!2,binding,type~BBaR0051p2)" <->
     "DNA(binding!1,type~BBaR0051p3,upstream!3), cI(dna!1), DNA(downstream!3,binding,type~BBaR0051p2)") :@ (0.00996323269897635,2.24),

    // cI binding to R0051p3 (cI bound){77261}
    ("DNA(binding,type~BBaR0051p3,upstream!2), cI(dna!1), cI(dna), DNA(downstream!2,binding!1,type~BBaR0051p2)" <->
     "DNA(binding!1,type~BBaR0051p3,upstream!3), cI(dna!2), cI(dna!1), DNA(downstream!3,binding!2,type~BBaR0051p2)") :@ (0.00996323269897635,0.09),


    // -- RNAP binding to promoter regions --

    // RNAP binding to R0010 (no LacI){77312}
    """DNA(binding,type~BBaR0010p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0010p4), RNAP(dna,rna),
       DNA(downstream!2,binding,type~BBaR0010p2)""" ->
    """DNA(binding,type~BBaR0010p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0010p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding,type~BBaR0010p2)""" :@ 0.000714031676759972,

    // RNAP binding to R0010 (LacI on p2){77313}
    """DNA(binding,type~BBaR0010p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0010p4), RNAP(dna,rna),
       DNA(downstream!2,binding!3,type~BBaR0010p2), LacI(dna!3)""" ->
    """DNA(binding,type~BBaR0010p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0010p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding!4,type~BBaR0010p2), LacI(dna!4)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0010 (LacI on p3){77314}
    """DNA(binding!3,type~BBaR0010p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0010p4), RNAP(dna,rna),
       DNA(downstream!2,binding,type~BBaR0010p2), LacI(dna!3)""" ->
    """DNA(binding!4,type~BBaR0010p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0010p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding,type~BBaR0010p2), LacI(dna!4)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0010 (LacI on p2 and p3){77315}
    """DNA(binding!3,type~BBaR0010p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0010p4), RNAP(dna,rna),
       DNA(downstream!2,binding!4,type~BBaR0010p2), LacI(dna!3), LacI(dna!4)""" ->
    """DNA(binding!4,type~BBaR0010p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0010p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding!5,type~BBaR0010p2), LacI(dna!4), LacI(dna!5)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0040 (no TetR){77301}
    """DNA(binding,type~BBaR0040p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0040p4), RNAP(dna,rna),
       DNA(downstream!2,binding,type~BBaR0040p2)""" ->
    """DNA(binding,type~BBaR0040p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0040p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding,type~BBaR0040p2)""" :@ 0.000714031676759972,

    // RNAP binding to R0040 (TetR on p2){77302}
    """DNA(binding,type~BBaR0040p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0040p4), RNAP(dna,rna),
       DNA(downstream!2,binding!3,type~BBaR0040p2), TetR(dna!3)""" ->
    """DNA(binding,type~BBaR0040p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0040p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding!4,type~BBaR0040p2), TetR(dna!4)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0040 (TetR on p3){77303}
    """DNA(binding!3,type~BBaR0040p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0040p4), RNAP(dna,rna),
       DNA(downstream!2,binding,type~BBaR0040p2), TetR(dna!3)""" ->
    """DNA(binding!4,type~BBaR0040p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0040p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding,type~BBaR0040p2), TetR(dna!4)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0040 (TetR on p2 and p3){77304}
    """DNA(binding!3,type~BBaR0040p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0040p4), RNAP(dna,rna),
       DNA(downstream!2,binding!4,type~BBaR0040p2), TetR(dna!3), TetR(dna!4)""" ->
    """DNA(binding!4,type~BBaR0040p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0040p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding!5,type~BBaR0040p2), TetR(dna!4), TetR(dna!5)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0051 (no cI){77266}
    """DNA(binding,type~BBaR0051p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0051p4), RNAP(dna,rna),
       DNA(downstream!2,binding,type~BBaR0051p2)""" ->
    """DNA(binding,type~BBaR0051p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0051p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding,type~BBaR0051p2)""" :@ 0.000714031676759972,

    // RNAP binding to R0051 (cI on p2){77267}
    """DNA(binding,type~BBaR0051p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0051p4), RNAP(dna,rna),
       DNA(downstream!2,binding!3,type~BBaR0051p2), cI(dna!3)""" ->
    """DNA(binding,type~BBaR0051p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0051p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding!4,type~BBaR0051p2), cI(dna!4)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0051 (cI on p3){77268}
    """DNA(binding!3,type~BBaR0051p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0051p4), RNAP(dna,rna),
       DNA(downstream!2,binding,type~BBaR0051p2), cI(dna!3)""" ->
    """DNA(binding!4,type~BBaR0051p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0051p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding,type~BBaR0051p2), cI(dna!4)""" :@ 7.14031676759972e-07,

    // RNAP binding to R0051 (cI on p2 and p3){77269}
    """DNA(binding!3,type~BBaR0051p3,upstream!2,downstream!1), DNA(upstream!1,binding,type~BBaR0051p4), RNAP(dna,rna),
       DNA(downstream!2,binding!4,type~BBaR0051p2), cI(dna!3), cI(dna!4)""" ->
    """DNA(binding!4,type~BBaR0051p3,upstream!3,downstream!1), DNA(upstream!1,binding!2,type~BBaR0051p4), RNAP(dna!2,rna),
       DNA(downstream!3,binding!5,type~BBaR0051p2), cI(dna!4), cI(dna!5)""" :@ 7.14031676759972e-07,


    // -- Protein Degradation --

    // LacI degradation{77288}
    "LacI(dna)" -> "" :@ 0.00115,

    // TetR degradation{77287}
    "TetR(dna)" -> "" :@ 0.00115,

    // cI degradation{77286}
    "cI(dna)" -> "" :@ 0.00115
  )


  // --- Initial conditions ---

  initialRNAPs = 700
  initialRibosomes = 18000

  // Transcription factors
  withObs("TetR", "TetR()")
  withObs("cI", "cI()")
  withObs("LacI", "LacI()")

  // maxEvents = 100
  maxTime = 12500
  run
}

object RepressilatorKBBFModelMain {
  def main(args: Array[String]): Unit = {
    // Make an hexagonal grid
    // Run each model in the grid and collect observables
    // Compute how many molecules are transported
    // Delete transported agent from source and add them to target
    new RepressilatorKBBFModel
  }
}

