package at.forsyte.harrsh.entailment

/**
  * Created by jkatelaa on 5/25/17.
  */
trait IterationAndEquivalenceSensitiveLearningStrategy extends LearningStrategy {

  override def iterationPostprocessing(obs : ObservationTable) : ObservationTable = obs.mergeDuplicateEntries

  override def checkPartition(partition: SymbolicHeapPartition, obs: ObservationTable, it: Int, entailmentLog: EntailmentLearningLog): ObservationTable = {
    obs.findEntryWithEquivalentRepFromIteration(partition.rep, it) match {
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
}
