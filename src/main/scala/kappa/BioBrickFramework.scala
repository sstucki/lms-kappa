package kappa


/** A class representing generic models. */
abstract class BioBrickFramework extends KappaModel {

  // Base contact graph on top which the user can add stuff
  val contactGraph: ContactGraph = """
    DNA(downstream!{1}, upstream!{1}, binding!{3}, type),
    RNA(downstream!{2,7}, upstream!{2},binding!{8}, type),
    RNAP(dna!{3}, rna!{7}),
    Ribosome(rna!{8}),
  """

  // TODO: There is an implicit volume in here that we should make explicit
  // var volume: Double = E coli avg volume

  var parts: Map[String, BioBrickPart] = Map()

  val l = AbstractKappaLinkState

  abstract class BioBrickPart {

    val partId: String

    def mkPart: AbstractPattern = s"DNA(type~${partId}, " +
        "upstream/upstream, downstream/downstream, binding/binding)"
  }

  // TODO: We should not have default values actually
  // Change RepressilatorKBBF.scala

  class Promoter(val partId: String) extends BioBrickPart {

    // Promoter binding sites for RNAP and TFs
    var _bindingSites: Int = 1

    // +1 is for backward compatibility with Ty's original framework
    def bindingSites: Int = _bindingSites + 1
    def bindingSites_=(n: Int) = _bindingSites = n

    // Transcription initiation rate
    var transcriptionRate: Double = 10.0

    override def mkPart: AbstractPattern = {
      val segments = for (i <- 1 to bindingSites) yield
        s"""DNA(type~${partId}p${i}, upstream/upstream,
          downstream/downstream, binding/p${i})""": AbstractPattern
      chain(segments, "upstream", "downstream")
    }

    override def toString = "Promoter(partId=" + partId +
      ",bindingSites=" + bindingSites + ")"
  }

  // object Promoter {
  //   def apply(partId: String) = new Promoter(partId)
  // }


  // BBaB0034: Fairly standard RBS
  class RBS(val partId: String = "BBaB0034") extends BioBrickPart {

    // RBS Ribosome binding rate
    var ribosomeBindingRate: Double = 0.000166053878316273

    // Translation initiation rate
    var translationRate: Double = 0.167

    // Transcription rate
    var transcriptionRate: Double = 10.0

    override def toString = "RBS(partId=" + partId + ")"
  }

  // object RBS {
  //   def apply(partId: String) = new RBS(partId)
  // }


  class CodingSequence(val partId: String) extends BioBrickPart {

    // Protein agent
    var _protein = AbstractPattern()

    def protein =
      if (_protein.isEmpty)
        throw new IllegalStateException("protein not defined")
      else _protein
    def protein_=(p: AbstractPattern) = _protein = p

    // Transcription rate
    var transcriptionRate: Double = 10.0

    // Translation rate
    var translationRate: Double = 10.0

    override def toString = "Promoter(partId=" + partId +
      ",protein=" + protein + ")"
  }

  // object CodingSequence {
  //   def apply(partId: String) = new CodingSequence(partId)
  // }


  // BBaB0011: Fairly standard terminator
  class Terminator(val partId: String = "BBaB0011")
      extends BioBrickPart {

    // Terminator falloff rate
    var falloffRate: Double = 10.0

    override def toString = "Terminator(partId=" + partId + ")"
  }

  // object Terminator {
  //   def apply(partId: String) = new Terminator(partId)
  // }


  def withPart(part: BioBrickPart): BioBrickPart = {
    parts += ((part.partId, part))
    part
  }

  def withParts(parts: BioBrickPart*): Seq[BioBrickPart] =
    parts map withPart


  var devices: Seq[Device] = List()

  class Device {

    private var _promoter = new Promoter("")
    private var _rbs = new RBS()
    private var _codingSeq = new CodingSequence("")
    private var _terminator = new Terminator()


    // - Promoter setters and getter -
    def promoter_=(partId: String) =
      parts(partId) match {
        case p: Promoter => _promoter = p
        case _ => throw new IllegalArgumentException(
          "promoter not found")
      }

    def promoter_=(p: Promoter) = _promoter = p

    def promoter: Promoter =
      if (_promoter.partId == "")
        throw new IllegalStateException("promoter not defined")
      else _promoter

    // - RBS setters and getter -
    def rbs_=(partId: String) =
      parts(partId) match {
        case rbs: RBS => _rbs = rbs
        case _ => throw new IllegalArgumentException(
          "RBS not found")
      }

    def rbs_=(rbs: RBS) = _rbs = rbs

    def rbs: RBS =
      if (_rbs.partId == "")
        throw new IllegalStateException("RBS not defined")
      else _rbs

    // - Coding sequence setters and getter -
    def codingSequence_=(partId: String) =
      parts(partId) match {
        case cs: CodingSequence => _codingSeq = cs
        case _ => throw new IllegalArgumentException(
          "coding sequence not found")
      }

    def codingSequence_=(cs: CodingSequence) = _codingSeq = cs

    def codingSequence: CodingSequence =
      if (_codingSeq.partId == "")
        throw new IllegalStateException("coding sequence not defined")
      else _codingSeq

    // - Terminator setters and getter -
    def terminator_=(partId: String) =
      parts(partId) match {
        case t: Terminator => _terminator = t
        case _ => throw new IllegalArgumentException(
          "terminator not found")
      }

    def terminator_=(t: Terminator) = _terminator = t

    def terminator: Terminator =
      if (_terminator.partId == "")
        throw new IllegalStateException("terminator not defined")
      else _terminator

    // devices = devices :+ this
  }

  def withDevice(dev: Device): Device = {
    devices = devices :+ dev
    dev
  }

  def withDevices(devs: Device*): Seq[Device] =
    devs map withDevice


  // -- Variables --

  var rnapFalloffRate = 1.0
  var ribosomeFalloffRate = 0.01
  var rnaDegradationRate = 0.0058
  var initialRNAPs = 0
  var initialRibosomes = 0

  override def initialise {

    // -- Generic Rules --

    withRules(
      // RNAP falloff
      "DNA(binding!1,downstream!3), RNAP(dna!1,rna!2), RNA(downstream!2), DNA(upstream!3,binding!_)" ->
      "DNA(binding,downstream!1), RNAP(dna,rna), RNA(downstream), DNA(upstream!1,binding!_)" :@ rnapFalloffRate,

      // Ribosome falloff
      "Ribosome(rna!1), RNA(binding!1)" -> "Ribosome(rna), RNA(binding)" :@ ribosomeFalloffRate,

      // RNA degradation
      "RNA(binding,downstream)" -> "" :@ rnaDegradationRate
    )

    for (part <- parts.values) part match {

      case p: Promoter => {
        val promoter = p.mkPart
        val rnapPos = p.bindingSites

        withRules(
          // Transcription (initiation)
          s"DNA(binding!1,type~${p.partId}p${rnapPos},downstream!2), RNAP(dna!1,rna  ), DNA(upstream!2,binding  )" ->
        s"""DNA(binding  ,type~${p.partId}p${rnapPos},downstream!3), RNAP(dna!1,rna!2), DNA(upstream!3,binding!1),
            RNA(binding,upstream,downstream!2,type~${p.partId})""" :@ p.transcriptionRate,

          // RNAP readthrough
          // FIXME: The rate is probably wrong because here we are jumping over many promoter
          // segments and p.transcriptionRate is the rate for jumping over just one.
          (promoter -< ("p1", l, l, "dna") + ("downstream", l, l, "upstream") >-
            "RNAP(dna/dna,rna!1), RNA(downstream!1), DNA(upstream/upstream,binding)").closeAll ->
          (promoter -< ("downstream", l, l, "upstream") >-
            s"""RNAP(dna!1,rna!2), RNA(downstream!3), DNA(upstream/upstream,binding!1),
                RNA(upstream!3,downstream!2,binding,type~${p.partId})""").closeAll :@ p.transcriptionRate
        )
      }

      case rbs: RBS => {
        withRules(
          // Transcription
          s"DNA(binding!1,downstream!2,type~${rbs.partId}), RNAP(dna!1,rna!3), DNA(upstream!2,binding  ), RNA(downstream!3)" ->
        s"""DNA(binding  ,downstream!2,type~${rbs.partId}), RNAP(dna!1,rna!3), DNA(upstream!2,binding!1), RNA(downstream!4),
            RNA(binding,upstream!4,downstream!3,type~${rbs.partId})""" :@ rbs.transcriptionRate,

          // Ribosome binding
          s"RNA(type~${rbs.partId},binding), Ribosome(rna)" ->
          s"RNA(type~${rbs.partId},binding!1), Ribosome(rna!1)" :@ rbs.ribosomeBindingRate,

          // Translation (initiation)
          s"RNA(type~${rbs.partId},downstream!1,binding!2), RNA(upstream!1,binding  ), Ribosome(rna!2)" ->
          s"RNA(type~${rbs.partId},downstream!1,binding  ), RNA(upstream!1,binding!2), Ribosome(rna!2)" :@ rbs.translationRate
        )
      }

      case cs: CodingSequence => {
        withRules(
          // Translation
          // FIXME: ++ is not a member of RuleBuilder (operator precedence? associativity?)
           s"RNA(type~${cs.partId},binding!1), Ribosome(rna!1)" ->
          (s"RNA(type~${cs.partId},binding  ), Ribosome(rna  )" ++ cs.protein) :@ cs.translationRate,

          // Transcription
          s"DNA(binding!1,downstream!2,type~${cs.partId}), RNAP(dna!1,rna!3), DNA(upstream!2,binding  ), RNA(downstream!3)" ->
        s"""DNA(binding  ,downstream!2,type~${cs.partId}), RNAP(dna!1,rna!3), DNA(upstream!2,binding!1), RNA(downstream!4),
            RNA(binding,upstream!4,downstream!3,type~${cs.partId})""" :@ cs.transcriptionRate
        )
      }

      case t: Terminator => {
        withRules(
          // Transcription
          s"DNA(binding!1,type~${t.partId}), RNAP(dna!1,rna!2), RNA(downstream!2)" ->
          s"DNA(binding  ,type~${t.partId}), RNAP(dna  ,rna  ), RNA(downstream  )" :@ t.falloffRate
        )
      }
    }


    // --- Initial mixture ---

    for (dev <- devices) {
      val promoter = dev.promoter.mkPart
      val rbs = dev.rbs.mkPart
      val codingSeq = dev.codingSequence.mkPart
      val terminator = dev.terminator.mkPart

      // FIXME: Operator associativity
      withInit((((
        promoter -< ("downstream", l, l, "upstream") >-
        rbs) -< ("downstream", l, l, "upstream") >-
        codingSeq) -< ("downstream", l, l, "upstream") >-
        terminator).closeAll)
    }

    withInit(initialRNAPs, "RNAP(dna,rna)")
    withInit(initialRibosomes, "Ribosome(rna)")

    super.initialise
  }
}

