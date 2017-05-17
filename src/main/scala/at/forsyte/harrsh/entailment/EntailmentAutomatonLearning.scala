package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
import at.forsyte.harrsh.util.{Combinators, IOUtils}

/**
  * Created by jens on 4/25/17.
  */
object EntailmentAutomatonLearning extends HarrshLogging {

  val FindOnlyNonEmpty = true // Only generate non-empty left-hand sides. Still have to figure out if this is the right approach
  val CleanUpSymbolicHeaps = true // Remove redundant variables / (in)equalities [good for performance and readability of the final results, but may complicate debugging]
  val ReportMCProgress = false

  type State = (ObservationTable, BreadthFirstUnfoldingsIterator, EntailmentLearningLog)

  def learnAutomaton(sid : SID, maxNumFv : Int, reportProgress : Boolean, maxIterations : Int = Int.MaxValue): (ObservationTable,EntailmentLearningLog) = {

    def learningIteration(it : Int, s : State) : State = {
      val (prevObs, unfoldingContinuation, entailmentLog) = s
      entailmentLog.printProgress("Beginning iteration " + it)
      logger.debug("Computation so far:\n" + entailmentLog)

      val (newUnfs, nextContinuation) = unfoldingContinuation.continue
      // Extend the observation table with the new observations (unfoldings)
      val extendedTable = processUnfoldings(newUnfs, prevObs, it, entailmentLog)
      // It can happen that we generate two representatives of the same class
      // We merge the corresponding table entries before continuing with the next iteration
      val cleanedTable = extendedTable.mergeDuplicateEntries
      (cleanedTable, nextContinuation, entailmentLog)
    }

    val entailmentLearningLog = new EntailmentLearningLog(reportProgress)
    val initialState = (ObservationTable.empty(sid, entailmentLearningLog),
                        BreadthFirstUnfoldingsIterator(sid, iteration = 1, continuation = Seq(sid.callToStartPred), maxNumFv, entailmentLearningLog),
                        entailmentLearningLog)

    val fixedPoint = Combinators.fixedPointComputation[State](initialState, (a,b) => a._1.entries.nonEmpty && a._1 == b._1, maxIterations)(learningIteration)
    fixedPoint._3.logEvent(EntailmentLearningLog.ReachedFixedPoint())
    (fixedPoint._1, fixedPoint._3)
  }

  private def processUnfoldings(partitions : Seq[SymbolicHeapPartition], obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable = {
    entailmentLog.printProgress("Commencing processing of " + partitions.size + " partitions for current iteration " + it)
    entailmentLog.printProgress(partitions.mkString("\n"))
    partitions.foldLeft(obs) {
      case (interObs, unf) => processUnfolding(unf, interObs, it, entailmentLog)
    }
  }

  private def processUnfolding(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable = {

    logger.debug("Processing Partition: " + partition)
    entailmentLog.logEvent(EntailmentLearningLog.ProcessPartition(partition))

    obs.findMatchingEntryFromIteration(partition.rep, it) match {
      case Some(entry) =>
        // Found an entry from the current iteration that contains the representative,
        // so we add the new extension
        extendEntryWithPartition(obs, entry, partition, it)
      case None =>
        // This exact representative is *not* in the table,
        // add it unless there is already an entry in the table that covers the representative
        if (!obs.hasEntryForEquivalenceClassOf(partition.rep)) {
          // The representative is genuinely new. We will keep it for the time being.
          // It might have to be merged with some other entry at the end of the iteration, though
          withNewTableEntryFromPartition(obs, partition, it)
        } else {
          obs
        }
    }
  }

  private def withNewTableEntryFromPartition(obs: ObservationTable, partition: SymbolicHeapPartition, it: Int): ObservationTable = {
    val cleanedPartition = if (EntailmentAutomatonLearning.CleanUpSymbolicHeaps) partition.simplify else partition
    obs.addNewEntryForPartition(cleanedPartition, it)
  }

  private def extendEntryWithPartition(obs: ObservationTable, entry: TableEntry, partition: SymbolicHeapPartition, it: Int): ObservationTable = {

    // New extension for the same representative, extend entry
    logger.debug("Entry for " + partition.rep + " is from current iteration => Add new extension " + partition.ext + " to entry")
    // FIXME Is the assertion below actually true? Or do we have to do some renaming instead?
    if (partition.repParamInstantiation != entry.repParamInstantiation) {
      IOUtils.printWarningToConsole("Difference in renaming:\nEntry: " + entry + "\nPartition: " + partition)
    }
    assert(partition.repParamInstantiation == entry.repParamInstantiation)
    obs.addExtensionToEntry(entry, partition.ext)

  }
}
