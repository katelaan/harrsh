package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog.RedEntCheck.ExtensionCompatibilityCheck
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConsistencyCheck
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}
import at.forsyte.harrsh.util.{Combinators, IOUtils}

/**
  * Created by jens on 4/25/17.
  */
object EntailmentAutomatonLearning extends HarrshLogging {

  val CleanUpSymbolicHeaps = true // Remove redundant variables / (in)equalities [good for performance and readability of the final results, but may complicate debugging]
  val ReportMCProgress = false

  type State = (ObservationTable, BreadthFirstUnfoldingsIterator, EntailmentLearningLog)

  def learnAutomaton(sid : SID, maxNumFv : Int, assumeAsymmetry : Boolean, reportProgress : Boolean, maxIterations : Int = Int.MaxValue): (ObservationTable,EntailmentLearningLog) = {

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

    val learningMode = if (assumeAsymmetry) {
      if (isStronglySymmetric(sid)) LearnStronglyAsymmetric() else LearnWeaklyAsymmetric()
    } else {
      LearnSymmetric()
    }
    IOUtils.printIf(reportProgress)("Using learning mode: " + learningMode)

    val entailmentLearningLog = new EntailmentLearningLog(reportProgress)
    entailmentLearningLog.logEvent(EntailmentLearningLog.LearningModeConfig(learningMode))

    val initialState = (ObservationTable.empty(learningMode, sid, entailmentLearningLog),
                        BreadthFirstUnfoldingsIterator(sid, learningMode, iteration = 1, continuation = Seq(sid.callToStartPred), maxNumFv, entailmentLearningLog),
                        entailmentLearningLog)

    val fixedPoint = Combinators.fixedPointComputation[State](initialState, (a,b) => a._1.entries.nonEmpty && a._1 == b._1, maxIterations)(learningIteration)
    fixedPoint._3.logEvent(EntailmentLearningLog.ReachedFixedPoint())
    (fixedPoint._1, fixedPoint._3)
  }

  private def isStronglySymmetric(sid : SID) : Boolean = {
    // TODO Is there a meaningful notion of strong asymmetry (where even the emp classes are guaranteed to behave in an asymmetric way)
    //!sid.rules.exists(rule => rule.body.isReduced && !rule.body.hasPointer)
    false
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

        obs.getUnfinishedMatchingEntry(partition.rep, it) match {
          case Some(entry) =>
            // 1.) There is an entry with matching extensions in the current iteration => add representative to that entry
            obs.addRepresentativeToEntry(entry, partition.rep)
          case None =>
            if (obs.hasMatchingEntryFromPreviousIterations(partition.rep, it)) {
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

    if (entry.reps.size == 1) {
      // Just one representative => new extension for the same representative => simply extend entry
      obs.addExtensionToEntry(entry, partition.ext, partition.extPredCall)
    } else {
      logger.debug("Entry has " + entry.reps.size + " representatives => consider splitting in two")

      // There is more than one representative
      // The new extension only corresponds to one of those representatives
      // => We might have to split the entry depending on entailment versus the new extension
      val (compatibleWithNewExtension, incompatibleWithNewExtension) = entry.reps.partition {
        rep =>
          val combination = SymbolicHeapPartition(rep, partition.ext, partition.extPredCall).recombined
          // If the combination is inconsistent, we conclude incompatibility,
          // since inconsistent heaps are never SID unfoldings
          // (recall that our algorithm assumes that all unfoldings of the SID are satisfiable!)
          if (!ConsistencyCheck.isConsistent(combination)) {
            logger.debug("Checking compatibility of " + rep + " with " + partition.ext + " => Incompatible, since combination " + combination + " is inconsistent")
            false
          } else {
            logger.debug("Checking compatibility of " + rep + " with " + partition.ext + " (combination: " + combination + ")")
            reducedEntailmentWithLogging(combination, obs.sid.callToStartPred, obs.sid, ExtensionCompatibilityCheck(rep, partition.ext), entailmentLog, entailmentLog.reportProgress && ReportMCProgress)
          }
      }

      if (incompatibleWithNewExtension.isEmpty) {
        logger.debug("No splitting necessary")
        obs.addExtensionToEntry(entry, partition.ext, partition.extPredCall)
      } else {
        logger.debug("Splitting into compatible reps " + compatibleWithNewExtension.mkString(", ") + " and incomptaible reps " + incompatibleWithNewExtension.mkString(", "))
        obs.splitEntry(entry, compatibleWithNewExtension, incompatibleWithNewExtension, partition.ext, partition.extPredCall)
      }
    }

  }

  private[entailment] def reducedEntailmentWithLogging(lhs : SymbolicHeap, rhs : SymbolicHeap, sid: SID, reason : EntailmentLearningLog.RedEntCheck.CheckPurpose, learningLog : EntailmentLearningLog, reportProgress : Boolean): Boolean = {
    learningLog.logEvent(EntailmentLearningLog.RedEntCheck(lhs, rhs, reason))
    ReducedEntailment.checkSatisfiableRSHAgainstSID(lhs, rhs, sid, reportProgress = reportProgress)
  }

  sealed trait LearningMode {

    override def toString: String = this match {
      case LearnWeaklyAsymmetric() => "weakly assymmetric (skip 'emp' during learning, perform full postprocessing)"
      case LearnStronglyAsymmetric() => "strongly assymmetric (perform renaming postprocessing)"
      case LearnSymmetric() => "symmetric (integrate renaming in the learning process)"
    }

    /**
      * Is the set of classes learned in this mode closed under renaming of free variables
      */
    def closedUnderParameterRenaming : Boolean = this match {
      case LearnWeaklyAsymmetric() => false
      case LearnStronglyAsymmetric() => false
      case LearnSymmetric() => true
    }

    /**
      * Should the learning algorithm consider unfoldings with empty spatial part?
      */
    def closedUnderEmp : Boolean = this match {
      case LearnWeaklyAsymmetric() => false
      case LearnStronglyAsymmetric() => true
      case LearnSymmetric() => true
    }
  }
  case class LearnWeaklyAsymmetric() extends LearningMode
  case class LearnStronglyAsymmetric() extends LearningMode
  case class LearnSymmetric() extends LearningMode
}
