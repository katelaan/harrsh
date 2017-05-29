package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment._
import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
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

  def learnAutomaton(sid: SID, maxNumFv: Int, assumeAsymmetry: Boolean, useSimpleLearning: Boolean, reportProgress: Boolean, maxIterations: Int = Int.MaxValue): (ObservationTable, EntailmentLearningLog) = {

    val learningAlgorithm: AutomatonLearningTemplate = (useSimpleLearning, assumeAsymmetry) match {
      case (true, true) => new AutomatonLearningTemplate with SimpleLearningStrategy with CloseResultUnderSymmetries
      case (true, false) => new AutomatonLearningTemplate with SimpleLearningStrategy with ClosePartitionsUnderSymmetries
      case (false, true) => new AutomatonLearningTemplate with IterationAndEquivalenceSensitiveLearningStrategy with CloseResultUnderSymmetries
      case (false, false) => new AutomatonLearningTemplate with IterationAndEquivalenceSensitiveLearningStrategy with ClosePartitionsUnderSymmetries
    }

    learningAlgorithm.run(sid, maxNumFv, reportProgress, maxIterations)
  }

  trait AutomatonLearningTemplate extends LearningComponent {

    self: LearningStrategy with SymmetryHandler =>

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

      // TODO Should actually check for equivalence of representatives? And in any case, enable configuration of when to establish normal form/check for equivalence?
      val obs = fixedPoint._1
      val cleaned = obs.copy(entries = obs.entries.map(cleanupEntry))

      // TODO Postprocessing
      // symmetryPostProcessing

      (cleaned, learningLog)
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

    private def cleanupEntry(entry : TableEntry) : TableEntry = {
      entry.copy(reps = entry.reps map cleanupHeap)
    }

    private def cleanupHeap(sh : SymbolicHeap) : SymbolicHeap = {
      // TODO Other normalization: Rename bound variables in the order they are discovered
      sh.copy(pure = sh.pure map (_.ordered))
    }

  }
}
