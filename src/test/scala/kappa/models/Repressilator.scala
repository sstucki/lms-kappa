package kappa.models

import kappa.KappaModel

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

class RepressilatorModel extends KappaModel {

  // R promoters C coding sequence
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

  withRules(
  // --- Transcription factor binding to promoter region ---

  // LacI binding to R0010p2 (no LacI){77308}
  ("DNA(binding,type~BBaR0010p3,upstream!2), LacI(dna,lactose), DNA(downstream!2,binding,type~BBaR0010p2)" <->
   "DNA(binding,type~BBaR0010p3,upstream!3), LacI(dna!1,lactose), DNA(downstream!3,binding!1,type~BBaR0010p2)") :@ (0.00996323269897635,2.24),

  // LacI binding to R0010p2 (LacI bound){77310}
  ("DNA(binding!1,type~BBaR0010p3,upstream!2), LacI(dna!1), DNA(downstream!2,binding,type~BBaR0010p2), LacI(dna,lactose)" <->
   "DNA(binding!2,type~BBaR0010p3,upstream!3), LacI(dna!2), DNA(downstream!3,binding!1,type~BBaR0010p2), LacI(dna!1,lactose)") :@ (0.00996323269897635,0.09),

  // LacI binding to R0010p3 (no LacI){77309}
  ("DNA(binding,type~BBaR0010p3,upstream!2), LacI(dna,lactose), DNA(downstream!2,binding,type~BBaR0010p2)" <->
   "DNA(binding!1,type~BBaR0010p3,upstream!3), LacI(dna!1,lactose), DNA(downstream!3,binding,type~BBaR0010p2)") :@ (0.00996323269897635,2.24),

  // LacI binding to R0010p3 (LacI bound){77311}
  ("DNA(binding,type~BBaR0010p3,upstream!2), LacI(dna!1), DNA(downstream!2,binding!1,type~BBaR0010p2), LacI(dna,lactose)" <->
   "DNA(binding!1,type~BBaR0010p3,upstream!3), LacI(dna!2), DNA(downstream!3,binding!2,type~BBaR0010p2), LacI(dna!1,lactose)") :@ (0.00996323269897635,0.09),

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


  // --- RNAP binding to promoter regions ---

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


  // --- Transcription ---

  // Transcription initiation of R0051{77270}
  "DNA(binding!1,type~BBaR0051p4,downstream!2), RNAP(dna!1,rna), DNA(upstream!2,binding)" ->
  "DNA(binding,type~BBaR0051p4,downstream!3), RNAP(dna!1,rna!2), DNA(upstream!3,binding!1), RNA(binding,upstream,downstream!2,type~BBaR0051)" :@ 10.0,

  // Transcription initiation of R0010{77316}
  "DNA(binding!1,type~BBaR0010p4,downstream!2), RNAP(dna!1,rna), DNA(upstream!2,binding)" ->
  "DNA(binding,type~BBaR0010p4,downstream!3), RNAP(dna!1,rna!2), DNA(upstream!3,binding!1), RNA(binding,upstream,downstream!2,type~BBaR0010)" :@ 10.0,

  // Transcription initiation of R0040{77305}
  "DNA(binding!1,type~BBaR0040p4,downstream!2), RNAP(dna!1,rna), DNA(upstream!2,binding)" ->
  "DNA(binding,type~BBaR0040p4,downstream!3), RNAP(dna!1,rna!2), DNA(upstream!3,binding!1), RNA(binding,upstream,downstream!2,type~BBaR0040)" :@ 10.0,

  // RBS BBa_B0034 transcription{77253}
  "DNA(binding!1,downstream!2,type~BBaB0034), RNAP(dna!1,rna!3), DNA(upstream!2,binding), RNA(downstream!3)" ->
  """DNA(binding,downstream!2,type~BBaB0034), RNAP(dna!1,rna!3), DNA(upstream!2,binding!1), RNA(downstream!4),
     RNA(binding,upstream!4,downstream!3,type~BBaB0034)""" :@ 10.0,

  // C0012 transcription{77279}
  "DNA(binding!1,downstream!2,type~BBaC0012), RNAP(dna!1,rna!3), DNA(upstream!2,binding), RNA(downstream!3)" ->
  """DNA(binding,downstream!2,type~BBaC0012), RNAP(dna!1,rna!3), DNA(upstream!2,binding!1), RNA(downstream!4),
     RNA(binding,upstream!4,downstream!3,type~BBaC0012)""" :@ 10.0,

  // C0051 transcription{77272}
  "DNA(binding!1,downstream!2,type~BBaC0051), RNAP(dna!1,rna!3), DNA(upstream!2,binding), RNA(downstream!3)" ->
  """DNA(binding,downstream!2,type~BBaC0051), RNAP(dna!1,rna!3), DNA(upstream!2,binding!1), RNA(downstream!4),
     RNA(binding,upstream!4,downstream!3,type~BBaC0051)""" :@ 10.0,

  // C0040 transcription{77278}
  "DNA(binding!1,downstream!2,type~BBaC0040), RNAP(dna!1,rna!3), DNA(upstream!2,binding), RNA(downstream!3)" ->
  """DNA(binding,downstream!2,type~BBaC0040), RNAP(dna!1,rna!3), DNA(upstream!2,binding!1), RNA(downstream!4),
     RNA(binding,upstream!4,downstream!3,type~BBaC0040)""" :@ 10.0,

  // - Termination and falloff -

  // Termination - B0011{77290}
  "DNA(binding!1,type~BBaB0011), RNAP(dna!1,rna!2), RNA(downstream!2)" ->
  "DNA(binding,type~BBaB0011), RNAP(dna,rna), RNA(downstream)" :@ 10.0,

  // RNAP falloff{76542}
  "DNA(binding!1,downstream!3), RNAP(dna!1,rna!2), RNA(downstream!2), DNA(upstream!3,binding!_)" ->
  "DNA(binding,downstream!1), RNAP(dna,rna), RNA(downstream), DNA(upstream!1,binding!_)" :@ 1.0,

  // - Readthrough -

  // Transcription of R0051 (readthrough){77271}
  """DNA(binding,type~BBaR0051p3,downstream!2,upstream!3), RNAP(dna!1,rna!5), DNA(upstream!6,binding), DNA(upstream!4,downstream!3,binding,type~BBaR0051p2),
     DNA(downstream!4,binding!1,type~BBaR0051p1), RNA(downstream!5), DNA(upstream!2,downstream!6,binding,type~BBaR0051p4)""" ->
  """DNA(binding,type~BBaR0051p3,downstream!3,upstream!5), RNAP(dna!1,rna!6), DNA(upstream!7,binding!1), DNA(upstream!4,downstream!5,binding,type~BBaR0051p2),
     DNA(downstream!4,binding,type~BBaR0051p1), RNA(downstream!2), DNA(upstream!3,downstream!7,binding,type~BBaR0051p4),
     RNA(binding,upstream!2,downstream!6,type~BBaR0051)""" :@ 10.0,

  // Transcription of R0010 (readthrough){77317}
  """DNA(binding,type~BBaR0010p3,downstream!2,upstream!3), RNAP(dna!1,rna!5), DNA(upstream!6,binding), DNA(upstream!4,downstream!3,binding,type~BBaR0010p2),
     DNA(downstream!4,binding!1,type~BBaR0010p1), RNA(downstream!5), DNA(upstream!2,downstream!6,binding,type~BBaR0010p4)""" ->
  """DNA(binding,type~BBaR0010p3,downstream!3,upstream!5), RNAP(dna!1,rna!6), DNA(upstream!7,binding!1), DNA(upstream!4,downstream!5,binding,type~BBaR0010p2),
     DNA(downstream!4,binding,type~BBaR0010p1), RNA(downstream!2), DNA(upstream!3,downstream!7,binding,type~BBaR0010p4),
     RNA(binding,upstream!2,downstream!6,type~BBaR0010)""" :@ 10.0,

  // Transcription of R0040 (readthrough){77306}
  """DNA(binding,type~BBaR0040p3,downstream!2,upstream!3), RNAP(dna!1,rna!5), DNA(upstream!6,binding), DNA(upstream!4,downstream!3,binding,type~BBaR0040p2),
     DNA(downstream!4,binding!1,type~BBaR0040p1), RNA(downstream!5), DNA(upstream!2,downstream!6,binding,type~BBaR0040p4)""" ->
  """DNA(binding,type~BBaR0040p3,downstream!3,upstream!5), RNAP(dna!1,rna!6), DNA(upstream!7,binding!1), DNA(upstream!4,downstream!5,binding,type~BBaR0040p2),
     DNA(downstream!4,binding,type~BBaR0040p1), RNA(downstream!2), DNA(upstream!3,downstream!7,binding,type~BBaR0040p4),
     RNA(binding,upstream!2,downstream!6,type~BBaR0040)""" :@ 10.0,


  // --- Translation ---

  // RBS BBa_B0034 Ribosome binding{77254}
  "RNA(binding,type~BBaB0034), Ribosome(rna)" ->
  "RNA(binding!1,type~BBaB0034), Ribosome(rna!1)" :@ 0.000166053878316273,

  // LacI translation initiation{77282}
  "RNA(binding!2,downstream!1), RNA(binding,upstream!1,type~BBaC0012), Ribosome(rna!2)" ->
  "RNA(binding,downstream!1), RNA(binding!2,upstream!1,type~BBaC0012), Ribosome(rna!2)" :@ 0.167,

  // TetR translation initiation{77281}
  "RNA(binding!2,downstream!1), RNA(binding,upstream!1,type~BBaC0040), Ribosome(rna!2)" ->
  "RNA(binding,downstream!1), RNA(binding!2,upstream!1,type~BBaC0040), Ribosome(rna!2)" :@ 0.167,

  // cI translation initiation{77280}
  "RNA(binding!2,downstream!1), RNA(binding,upstream!1,type~BBaC0051), Ribosome(rna!2)" ->
  "RNA(binding,downstream!1), RNA(binding!2,upstream!1,type~BBaC0051), Ribosome(rna!2)" :@ 0.167,

  // LacI translation{77285}
  "RNA(binding!1,type~BBaC0012), Ribosome(rna!1)" ->
  "RNA(binding,type~BBaC0012), Ribosome(rna), LacI(dna,lactose)" :@ 10.0,

  // TetR translation{77284}
  "RNA(binding!1,type~BBaC0040), Ribosome(rna!1)" ->
  "RNA(binding,type~BBaC0040), Ribosome(rna), TetR(dna,atc)" :@ 10.0,

  // cI translation{77283}
  "RNA(binding!1,type~BBaC0051), Ribosome(rna!1)" ->
  "RNA(binding,type~BBaC0051), Ribosome(rna), cI(dna)" :@ 10.0,

  // Ribosome falloff{76777}
  "Ribosome(rna!1), RNA(binding!1)" ->
  "Ribosome(rna), RNA(binding)" :@ 0.01,


  // --- Degradation ---

  // RNA degradation{76543}
  "RNA(binding,downstream)" -> "" :@ 0.0058,

  // LacI degradation{77288}
  "LacI(dna)" -> "" :@ 0.00115,

  // TetR degradation{77287}
  "TetR(dna)" -> "" :@ 0.00115,

  // cI degradation{77286}
  "cI(dna)" -> "" :@ 0.00115
  )


  // --- Initial conditions ---

  withInit(700, "RNAP(dna,rna)")
  withInit(18000, "Ribosome(rna)")

  // DNA
  withInit("""DNA(upstream,downstream!1,binding,type~BBaR0051p1), DNA(upstream!1,downstream!2,binding,type~BBaR0051p2),
              DNA(upstream!2,downstream!3,binding,type~BBaR0051p3), DNA(upstream!3,downstream!4,binding,type~BBaR0051p4),
              DNA(upstream!4,downstream!5,binding,type~BBaB0034), DNA(upstream!5,downstream!6,binding,type~BBaC0012),
              DNA(upstream!6,downstream,binding,type~BBaB0011)""")
  withInit("""DNA(upstream,downstream!1,binding,type~BBaR0010p1), DNA(upstream!1,downstream!2,binding,type~BBaR0010p2),
              DNA(upstream!2,downstream!3,binding,type~BBaR0010p3), DNA(upstream!3,downstream!4,binding,type~BBaR0010p4),
              DNA(upstream!4,downstream!5,binding,type~BBaB0034), DNA(upstream!5,downstream!6,binding,type~BBaC0040),
              DNA(upstream!6,downstream,binding,type~BBaB0011)""")
  withInit("""DNA(upstream,downstream!1,binding,type~BBaR0040p1), DNA(upstream!1,downstream!2,binding,type~BBaR0040p2),
              DNA(upstream!2,downstream!3,binding,type~BBaR0040p3), DNA(upstream!3,downstream!4,binding,type~BBaR0040p4),
              DNA(upstream!4,downstream!5,binding,type~BBaB0034), DNA(upstream!5,downstream!6,binding,type~BBaC0051),
              DNA(upstream!6,downstream,binding,type~BBaB0011)""")

  // Transcription factors
  withObs("TetR", "TetR()")
  withObs("cI", "cI()")
  withObs("LacI", "LacI()")

  // maxEvents = 100
  maxTime = 12500
  run
}

object RepressilatorModelMain {
  def main(args: Array[String]): Unit = {
    // Make an hexagonal grid
    // Run each model in the grid and collect observables
    // Compute how many molecules are transported
    // Delete transported agent from source and add them to target
    new RepressilatorModel
  }
}

