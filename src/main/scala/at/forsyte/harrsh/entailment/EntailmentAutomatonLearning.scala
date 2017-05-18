package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog.RedEntCheck
import at.forsyte.harrsh.entailment.EntailmentLearningLog.RedEntCheck.ExtensionCompatibilityCheck
import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConsistencyCheck
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

      val (newPartitions, nextContinuation) = unfoldingContinuation.continue
      // Extend the observation table with the new observations (unfoldings)
      val extendedTable = processPartitions(newPartitions, prevObs, it, entailmentLog)
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

  private def processPartitions(partitions : Seq[SymbolicHeapPartition], obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable = {
    entailmentLog.printProgress("Commencing processing of " + partitions.size + " partitions for current iteration " + it)
    entailmentLog.printProgress(partitions.mkString("\n"))
    partitions.foldLeft(obs) {
      case (interObs, unf) => processSinglePartition(unf, interObs, it, entailmentLog)
    }
  }

  private def processSinglePartition(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable = {

    logger.debug("Processing Partition: " + partition)
    entailmentLog.logEvent(EntailmentLearningLog.ProcessPartition(partition))

    obs.findMatchingEntryFromIteration(partition.rep, it) match {
      case Some(entry) =>
        // Found an entry from the current iteration that contains the representative,
        // so we add the new extension
        logger.debug(entry + " containing " + partition.rep + " is from current iteration => Add new extension " + partition.ext + " to entry")
        enlargeEntryWithExtension(obs, entry, partition, it, entailmentLog)
      case None =>
        // This exact representative is *not* in the table
        // There are three cases:
        logger.debug("No exact match for entry " + partition.rep + " => trying reduction to known entry")

        obs.getEntryForEquivalenceClassFromCurrentIteration(partition.rep, it) match {
          case Some(entry) =>
            // 1.) There is an entry with matching extensions in the current iteration => add representative to that entry
            obs.addRepresentativeToEntry(entry, partition.rep)
          case None =>
            if (obs.hasEntryForEquivalenceClassFromPreviousIterations(partition.rep, it)) {
              // 2.) There is an entry with matching extensions in previous iteration => discard partition
              obs
            } else {
              // 3.) There is no entry with matching extensions =>
              // The representative is genuinely new. We will keep it for the time being.
              // It might have to be merged with some other entry at the end of the iteration, though
              withNewTableEntryFromPartition(obs, partition, it)
            }
        }
    }
  }

  private def withNewTableEntryFromPartition(obs: ObservationTable, partition: SymbolicHeapPartition, it: Int): ObservationTable = {
    val cleanedPartition = if (EntailmentAutomatonLearning.CleanUpSymbolicHeaps) partition.simplify else partition
    obs.addNewEntryForPartition(cleanedPartition, it)
  }

  private def enlargeEntryWithRepresentative(obs: ObservationTable, entry: TableEntry, partition: SymbolicHeapPartition, it: Int): ObservationTable = {
    obs.addRepresentativeToEntry(entry, partition.rep)
  }

  private def enlargeEntryWithExtension(obs: ObservationTable, entry: TableEntry, partition: SymbolicHeapPartition, it: Int, entailmentLog : EntailmentLearningLog): ObservationTable = {

    // FIXME Is the assertion below actually true? Or do we have to do some renaming instead?
    if (partition.repParamInstantiation != entry.repParamInstantiation) {
      IOUtils.printWarningToConsole("Difference in renaming:\nEntry: " + entry + "\nPartition: " + partition)
      assert(partition.repParamInstantiation == entry.repParamInstantiation)
    }

    if (entry.reps.size == 1) {
      // Just one representative => new extension for the same representative => simply extend entry
      obs.addExtensionToEntry(entry, partition.ext)
    } else {
      logger.debug("Entry has " + entry.reps.size + " representatives => consider splitting in two")

      // There is more than one representative
      // The new extension only corresponds to one of those representatives
      // => We might have to split the entry depending on entailment versus the new extension
      val (compatibleWithNewExtension, incompatibleWithNewExtension) = entry.reps.partition {
        rep =>
          // TODO The following will also have to change if the instantiations are non-identical?
          val combination = SymbolicHeap.mergeHeaps(rep.renameVars(entry.repParamInstantiation), partition.ext, partition.repParamInstantiation.codomain)
          // TODO Abstract away reduced entailment check with integrated logging?
          entailmentLog.logEvent(RedEntCheck(combination, obs.sid.callToStartPred, ExtensionCompatibilityCheck(rep, partition.ext)))

          // If the combination is inconsistent, we conclude incompatibility,
          // since inconsistent heaps are never SID unfoldings
          // (recall that our algorithm assumes that all unfoldings of the SID are satisfiable!)
          if (!ConsistencyCheck.isConsistent(combination)) {
            logger.debug("Checking compatibility of " + rep + " with " + partition.ext + " => Incompatible, since combination " + combination + " is inconsistent")
            false
          } else {
            logger.debug("Checking compatibility of " + rep + " with " + partition.ext + " (combination: " + combination + ")")
            ReducedEntailment.checkSatisfiableRSHAgainstSID(combination, obs.sid.callToStartPred, obs.sid, entailmentLog.reportProgress && ReportMCProgress)
          }
      }

      if (incompatibleWithNewExtension.isEmpty) {
        logger.debug("No splitting necessary")
        obs.addExtensionToEntry(entry, partition.ext)
      } else {
        logger.debug("Splitting into compatible reps " + compatibleWithNewExtension.mkString(", ") + " and incomptaible reps " + incompatibleWithNewExtension.mkString(", "))
        obs.splitEntry(entry, compatibleWithNewExtension, incompatibleWithNewExtension, partition.ext)
      }
    }

  }
}
