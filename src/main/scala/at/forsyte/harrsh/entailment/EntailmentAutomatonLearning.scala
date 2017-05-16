package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.GenerateEntailmentAutomata.logger
import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.seplog.inductive.{SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.util.{Combinators, IOUtils}

/**
  * Created by jens on 4/25/17.
  */
object EntailmentAutomatonLearning extends HarrshLogging {

  val FindOnlyNonEmpty = true // Only generate non-empty left-hand sides. Still have to figure out if this is the right approach
  val CleanUpSymbolicHeaps = true
  val ReportMCProgress = false

  def printProgress : String => Unit = x => println(x)

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

  private def processUnfoldings(partitions : Seq[SymbolicHeapPartition], obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable =
    partitions.foldLeft(obs){
      case (interObs, unf) => processUnfolding(unf, interObs, it, entailmentLog)
    }

  private def processUnfolding(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable = {

    logger.debug("Processing Partition: " + partition)
    entailmentLog.logEvent(EntailmentLearningLog.ProcessPartition(partition))

    obs.findEntryFromIterationWithEquivalentRepresentative(partition.rep, it) match {
      case Some(entry) =>
        // Found an entry from the current iteration that contains the representative,
        // so we add the new extension
        extendEntryWithPartition(entry, partition, obs, it)
      case None =>
        // This exact representative is *not* in the table,
        // add it unless there is already an entry in the table that covers the representative
        if (isIrreducible(partition.rep, obs)) {
          addNewEntryForPartition(partition, obs, it)
        } else {
          obs
        }
    }
  }

  private def isIrreducible(sh : SymbolicHeap, obs : ObservationTable): Boolean = {
    obs.findEntryForEquivalenceClassOf(sh).isEmpty
  }

  private def addNewEntryForPartition(partition: SymbolicHeapPartition, obs: ObservationTable, it: Loc): ObservationTable = {
    obs.findEntryForEquivalenceClassOf(partition.rep) match {
      case Some(reducedEntry) =>
        // Have already dealt with the same partition in the previous iteration, discard
        logger.debug("Can reduce " + partition.rep + " to " + reducedEntry.reps + ", discarding " + partition)
        obs
      case None =>
        // The representative is genuinely new. We will keep it for the time being.
        // It might have to be merged with some other entry at the end of the iteration, though
        val cleanedPartition = if (EntailmentAutomatonLearning.CleanUpSymbolicHeaps) partition.simplify else partition
        obs.addNewEntryForPartition(cleanedPartition, it)
    }
  }

  private def extendEntryWithPartition(entry: TableEntry, partition: SymbolicHeapPartition, obs: ObservationTable, it: Int): ObservationTable = {

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
