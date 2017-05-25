package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog.RedEntCheck.ExtensionCompatibilityCheck
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConsistencyCheck

/**
  * Created by jkatelaa on 5/25/17.
  */
trait LearningStrategy extends HarrshLogging {

  def iterationPostprocessing(obs : ObservationTable) : ObservationTable

  def checkPartition(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable

  protected def withNewTableEntryFromPartition(obs: ObservationTable, partition: SymbolicHeapPartition, it: Int): ObservationTable = {
    val cleanedPartition = if (EntailmentAutomatonLearning.CleanUpSymbolicHeaps) partition.simplify else partition
    obs.addNewEntryForPartition(cleanedPartition, it)
  }

  protected def enlargeEntryWithExtension(obs: ObservationTable, entry: TableEntry, partition: SymbolicHeapPartition, it: Int, entailmentLog : EntailmentLearningLog): ObservationTable = {

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
            EntailmentAutomatonLearning.reducedEntailmentWithLogging(combination, obs.sid.callToStartPred, obs.sid, ExtensionCompatibilityCheck(rep, partition.ext), entailmentLog, entailmentLog.reportProgress && EntailmentAutomatonLearning.ReportMCProgress)
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

}
