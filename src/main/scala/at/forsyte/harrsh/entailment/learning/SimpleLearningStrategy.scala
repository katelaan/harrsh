package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment._

/**
  * Created by jkatelaa on 5/25/17.
  */
trait SimpleLearningStrategy extends LearningStrategy {

  override def componentDescriptions : Seq[String] = "simple learning" +: super.componentDescriptions

  override def iterationPostprocessing(obs : ObservationTable) : ObservationTable = obs

  override def checkPartition(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int) : ObservationTable = {

    def updateEntry(entry : TableEntry) = {
      if (entry.discoveredInIteration < it) {
        logger.debug("Don't add " + partition + ", because entry is already closed")
        obs
      } else {
        logger.debug("Matching entry " + entry + " is still open, will add " + partition)
        val (obsWithRep,entryWithRep) = obs.addRepresentativeToEntryAndReturnEntry(entry, partition.rep)
        obsWithRep.addExtensionToEntry(entryWithRep, partition, it)
      }
    }

    val entries = obs.getAllMatchingEntries(partition.rep)
    entries.size match {
      case 0 =>
        // No match, new class
        obs.addNewEntryForPartition(partition, it)
      case 1 =>
        // One match => Extend the class (which might trigger a split though, if it actually represents more than one equivalence class!)
        updateEntry(entries.head)
      case i =>
        // Multiple entries match. This is ok, as long as the extensions sets of these entries form a semi-lattice,
        // i.e.
        logger.debug(i + " matching entries for " + partition + ":\n" + entries.mkString("\n"))
        // FIXME Right way to resolve ambiguity?
        val strongest = TableEntry.entryWithWeakestExtensions(entries)
        //val strongest = TableEntry.entryWithMoreExtensions(entries)
        logger.debug("Strongest entry (i.e., with weakest extensions): " + strongest)
        updateEntry(strongest)
    }
  }

}
