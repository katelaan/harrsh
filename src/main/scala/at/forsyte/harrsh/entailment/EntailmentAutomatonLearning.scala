package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.{Combinators, IOUtils}

/**
  * Created by jens on 4/25/17.
  */
object EntailmentAutomatonLearning extends HarrshLogging {

  val CleanUpSymbolicHeaps = true // Remove redundant variables / (in)equalities [good for performance and readability of the final results, but may complicate debugging]
  val ReportMCProgress = false

  type State = (ObservationTable, BreadthFirstUnfoldingsIterator, EntailmentLearningLog)

  // FIXME Partition generation: Should we always keep some emp classes (with nonempty pure part)?

  def learnAutomaton(sid : SID, maxNumFv : Int, assumeAsymmetry : Boolean, reportProgress : Boolean, maxIterations : Int = Int.MaxValue): (ObservationTable,EntailmentLearningLog) = {
    val learningAlgorithm : AutomatonLearningTemplate = if (assumeAsymmetry) {
      new AutomatonLearningTemplate(sid, maxNumFv, reportProgress, maxIterations) with /*SimpleLearningStrategy*/ IterationAndEquivalenceSensitiveLearningStrategy with RemoveEmpPartition with CloseResultUnderSymmetries
    } else {
      new AutomatonLearningTemplate(sid, maxNumFv, reportProgress, maxIterations) with SimpleLearningStrategy /*IterationAndEquivalenceSensitiveLearningStrategy*/ with RemoveEmpPartition with CloseUnfoldingsUnderSymmetries
    }
    learningAlgorithm.run
  }

  class AutomatonLearningTemplate(sid : SID, maxNumFv : Int, reportProgress : Boolean, maxIterations : Int = Int.MaxValue) {

    self: LearningStrategy with PartitionFilter with SymmetryHandler =>

    def run: (ObservationTable, EntailmentLearningLog) = {

      def learningIteration(it: Int, s: State): State = {
        val (prevObs, unfoldingContinuation, entailmentLog) = s
        entailmentLog.printProgress("Beginning iteration " + it)
        logger.debug("Computation so far:\n" + entailmentLog)

        val (newPartitions, nextContinuation) = unfoldingContinuation.continue
        // Extend the observation table with the new observations (unfoldings)
        val extendedTable = processPartitions(newPartitions, prevObs, it, entailmentLog)
        // It can happen that we generate two representatives of the same class
        // We merge the corresponding table entries before continuing with the next iteration
        val cleanedTable = iterationPostprocessing(extendedTable, entailmentLog)
        (cleanedTable, nextContinuation, entailmentLog)
      }

//      val learningMode = if (assumeAsymmetry) {
//        if (isStronglySymmetric(sid)) LearnStronglyAsymmetric() else LearnWeaklyAsymmetric()
//      } else {
//        LearnSymmetric()
//      }
//      IOUtils.printIf(reportProgress)("Using learning mode: " + learningMode)

      val entailmentLearningLog = new EntailmentLearningLog(reportProgress)
//      entailmentLearningLog.logEvent(EntailmentLearningLog.LearningModeConfig(learningMode))

      val initialState = (ObservationTable.empty(sid, entailmentLearningLog),
        BreadthFirstUnfoldingsIterator(sid, iteration = 1, continuation = Seq(sid.callToStartPred), maxNumFv, entailmentLearningLog, this),
        entailmentLearningLog)

      val fixedPoint = Combinators.fixedPointComputation[State](initialState, (a, b) => a._1.entries.nonEmpty && a._1 == b._1, maxIterations)(learningIteration)
      fixedPoint._3.logEvent(EntailmentLearningLog.ReachedFixedPoint())

      // TODO Postprocessing
      // symmetryPostProcessing

      (fixedPoint._1, fixedPoint._3)
    }

    private def isStronglySymmetric(sid: SID): Boolean = {
      // TODO Is there a meaningful notion of strong asymmetry (where even the emp classes are guaranteed to behave in an asymmetric way)
      //!sid.rules.exists(rule => rule.body.isReduced && !rule.body.hasPointer)
      false
    }

    private def processPartitions(partitions: Seq[SymbolicHeapPartition], obs: ObservationTable, it: Int, entailmentLog: EntailmentLearningLog): ObservationTable = {
      entailmentLog.printProgress("Commencing processing of " + partitions.size + " partitions for current iteration " + it)
      entailmentLog.printProgress(partitions.mkString("\n"))
      partitions.foldLeft(obs) {
        case (interObs, unf) => processSinglePartition(unf, interObs, it, entailmentLog)
      }
    }

    private def processSinglePartition(partition: SymbolicHeapPartition, obs: ObservationTable, it: Int, entailmentLog: EntailmentLearningLog): ObservationTable = {

      logger.debug("Processing Partition: " + partition)
      entailmentLog.logEvent(EntailmentLearningLog.ProcessPartition(partition))

      checkPartition(partition, obs, it, entailmentLog)
      //iterationAndEquivalenceBasedOptimizedProcessing(partition, obs, it, entailmentLog)
    }

  }

  private[entailment] def reducedEntailmentWithLogging(lhs : SymbolicHeap, rhs : SymbolicHeap, sid: SID, reason : EntailmentLearningLog.RedEntCheck.CheckPurpose, learningLog : EntailmentLearningLog, reportProgress : Boolean): Boolean = {
    learningLog.logEvent(EntailmentLearningLog.RedEntCheck(lhs, rhs, reason))
    ReducedEntailment.checkSatisfiableRSHAgainstSID(lhs, rhs, sid, reportProgress = reportProgress)
  }

//  sealed trait LearningMode {
//
//    override def toString: String = this match {
//      case LearnWeaklyAsymmetric() => "weakly assymmetric (skip 'emp' during learning, perform full postprocessing)"
//      case LearnStronglyAsymmetric() => "strongly assymmetric (perform renaming postprocessing)"
//      case LearnSymmetric() => "symmetric (integrate renaming in the learning process)"
//    }
//
//    /**
//      * Is the set of classes learned in this mode closed under renaming of free variables
//      */
//    def closedUnderParameterRenaming : Boolean = this match {
//      case LearnWeaklyAsymmetric() => false
//      case LearnStronglyAsymmetric() => false
//      case LearnSymmetric() => true
//    }
//
//    /**
//      * Should the learning algorithm consider unfoldings with empty spatial part?
//      */
//    def closedUnderEmp : Boolean = this match {
//      case LearnWeaklyAsymmetric() => false
//      case LearnStronglyAsymmetric() => true
//      case LearnSymmetric() => true
//    }
//  }
//  case class LearnWeaklyAsymmetric() extends LearningMode
//  case class LearnStronglyAsymmetric() extends LearningMode
//  case class LearnSymmetric() extends LearningMode
}
