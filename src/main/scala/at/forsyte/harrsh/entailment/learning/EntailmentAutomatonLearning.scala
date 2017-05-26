package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment._
import at.forsyte.harrsh.entailment.learning.PartitionFilter.RemoveEmpPartition
import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jens on 4/25/17.
  */
object EntailmentAutomatonLearning extends HarrshLogging {

  /**
    * Remove redundant variables / (in)equalities [good for performance and readability of the final results, but may complicate debugging]
    */
  val CleanUpSymbolicHeaps = true

  val ReportMCProgress = false

  type State = (ObservationTable, BreadthFirstUnfoldingsIterator)

  // FIXME Is RemoveEmpPartition a suitable partition filter?

  def learnAutomaton(sid: SID, maxNumFv: Int, assumeAsymmetry: Boolean, useSimpleLearning: Boolean, reportProgress: Boolean, maxIterations: Int = Int.MaxValue): (ObservationTable, EntailmentLearningLog) = {

    val learningAlgorithm: AutomatonLearningTemplate = (assumeAsymmetry, useSimpleLearning) match {
      case (true, true) => new AutomatonLearningTemplate with RemoveEmpPartition with SimpleLearningStrategy with CloseResultUnderSymmetries
      case (true, false) => new AutomatonLearningTemplate with RemoveEmpPartition with IterationAndEquivalenceSensitiveLearningStrategy with CloseResultUnderSymmetries
      case (false, true) => new AutomatonLearningTemplate with RemoveEmpPartition with SimpleLearningStrategy with CloseUnfoldingsUnderSymmetries
      case (false, false) => new AutomatonLearningTemplate with RemoveEmpPartition with IterationAndEquivalenceSensitiveLearningStrategy with CloseUnfoldingsUnderSymmetries
    }

    learningAlgorithm.run(sid, maxNumFv, reportProgress, maxIterations)
  }

  trait AutomatonLearningTemplate extends LearningComponent {

    self: LearningStrategy with PartitionFilter with SymmetryHandler =>

    def run(sid: SID, maxNumFv: Int, reportProgress: Boolean, maxIterations: Int = Int.MaxValue): (ObservationTable, EntailmentLearningLog) = {

      def learningIteration(it: Int, s: State): State = {
        val (prevObs, unfoldingContinuation) = s
        learningLog.printProgress("Beginning iteration " + it)

        val (newPartitions, nextContinuation) = unfoldingContinuation.continue
        // Extend the observation table with the new observations (unfoldings)
        val extendedTable = processPartitions(newPartitions, prevObs, it, learningLog)
        // It can happen that we generate two representatives of the same class
        // We merge the corresponding table entries before continuing with the next iteration
        val cleanedTable = iterationPostprocessing(extendedTable)
        (cleanedTable, nextContinuation)
      }

      learningLog.reportProgress = reportProgress
      learningLog.logEvent(EntailmentLearningLog.LearningModeConfig(componentDescriptions))

      val initialState = (ObservationTable.empty(sid, learningLog),
        BreadthFirstUnfoldingsIterator(sid, iteration = 1, continuation = Seq(sid.callToStartPred), maxNumFv, learningLog, this))

      val fixedPoint = Combinators.fixedPointComputation[State](initialState, (a, b) => a._1.entries.nonEmpty && a._1 == b._1, maxIterations)(learningIteration)
      learningLog.logEvent(EntailmentLearningLog.ReachedFixedPoint())

      // TODO Postprocessing
      // symmetryPostProcessing

      (fixedPoint._1, learningLog)
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

      checkPartition(partition, obs, it)
    }

  }
}
