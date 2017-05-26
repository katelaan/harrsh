package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog.{TableLookupOperation, TableOperations, TableUpdateOperation}
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 5/25/17.
  */
trait IterationAndEquivalenceSensitiveLearningStrategy extends LearningStrategy {

  self : LearningComponent =>

  override def iterationPostprocessing(obs : ObservationTable) : ObservationTable = mergeDuplicateEntries(obs)

  override def checkPartition(partition: SymbolicHeapPartition, obs: ObservationTable, it: Int): ObservationTable = {
    findEntryWithEquivalentRepFromIteration(obs, partition.rep, it) match {
      case Some(entry) =>
        // Found an entry from the current iteration that contains the representative,
        // so we add the new extension
        logger.debug(entry + " containing " + partition.rep + " is from current iteration => Add new extension " + partition.ext + " to entry")
        obs.addExtensionToEntry(entry, partition, it)
      case None =>
        // This exact representative is *not* in the table
        // There are three cases:
        logger.debug("No exact match for entry " + partition.rep + " => trying reduction to known entry")

        getUnfinishedMatchingEntry(obs, partition.rep, it) match {
          case Some(entry) =>
            // 1.) There is an entry with matching extensions in the current iteration => add representative to that entry
            obs.addRepresentativeToEntry(entry, partition.rep)
          case None =>
            if (hasMatchingEntryFromPreviousIterations(obs, partition.rep, it)) {
              // 2.) There is an entry with matching extensions in previous iteration => discard partition
              obs
            } else {
              // 3.) There is no entry with matching extensions =>
              // The representative is genuinely new. We will keep it for the time being.
              // It might have to be merged with some other entry at the end of the iteration, though
              obs.addNewEntryForPartition(partition, it)
            }
        }
    }
  }

  /**
    * Searches the set of entries for an entry from the given iteration whose set of representatives contains a
    * representative matching the given representative (i.e., an entry with a rep equivalent to the given heap)
    * @param representative Heap whose entry we try to find
    * @param iteration Only entries discovered in this iteration are searched
    * @return Some iteration with equivalent representative or None
    */
  private def findEntryWithEquivalentRepFromIteration(obs : ObservationTable, representative : SymbolicHeap, iteration : Int) : Option[TableEntry] = {
    val res = obs.entries.find(entry => entry.discoveredInIteration == iteration && entry.containsEquivalentRepresentative(representative))
    res.foreach(entry => learningLog.logEvent(TableLookupOperation(TableOperations.FoundEquivalent(representative, entry))))
    res
  }

  // FIXME This could be incorrect in some cases, because there is some nondeterminism left in the observation table! Idea: It might be possible to prioritize / only consider classes that do not lead to redundant pure constraints, but have to think this through
  // E.g. if emp { x1 != x2 } is in the language and there are classes with extensions emp and emp : {x1 != x2}, then the heap emp : {x1 != x2} would actually be placed in both classes...
  private def hasMatchingEntryFromPreviousIterations(obs : ObservationTable, sh : SymbolicHeap, currentIteration : Int) : Boolean = obs.findEntryForEquivalenceClassOf(sh, _.discoveredInIteration < currentIteration).nonEmpty

  /**
    * Returns some unifished entry containing rep (i.e., entry that can still be extended); an entry is unfinished if it is from the current iteration or if it was from the previous iteration and contains emp.
    * @param rep Representative whose entry we look for
    * @param currentIteration Current iteration
    * @return Some matching entry or nothing
    */
  private def getUnfinishedMatchingEntry(obs : ObservationTable, rep : SymbolicHeap, currentIteration : Int) : Option[TableEntry] = {
    // If the learning mode closes under emp, i.e. treats emp like every other representative, no special treatment of emp reps is necesary;
    // In that case, only entries from the current iteration are treated as unfinished
    // On the other hand, if emp needs special treatment, we might split off its own equivalence class later; we therefore need to find another representative of the class, which might happen only in the iteration after emp is discovered
    // TODO Is this really the right criterion? What if we disocver both emp and another representative in the same iteration? (I think we still need to consider the next iteration, but am not 100% sure)
    def isUnfinished(entry : TableEntry) = entry.discoveredInIteration == currentIteration || (entry.discoveredInIteration == currentIteration-1 && entry.hasEmptyRepresentative)
    obs.findEntryForEquivalenceClassOf(rep, isUnfinished)
  }

  private def mergeDuplicateEntries(obs : ObservationTable) : ObservationTable = {
    // Merge entries with identical sets of extensions
    // FIXME Will the extensions really be identical or just equivalent? Should come up with a counterexample where we need the latter!
    val grouped = obs.entries.groupBy(_.exts)
    val mergedEntries : Seq[TableEntry] = (grouped map {
      case (ext, groupedEntries) =>
        if (groupedEntries.size == 1)
          groupedEntries.head
        else {
          logger.debug("Performing merge of the following entries:\n" + groupedEntries.map(" - " + _).mkString("\n"))
          val byIteration = groupedEntries.groupBy(_.discoveredInIteration)
          if (byIteration.size > 1) {
            val minKey = byIteration.keySet.min
            // FIXME Is this always a correct thing to do? (Should be in the 1-predicate case, but in other cases?)
            logger.debug("Trivial merge: Discarding all entries found in iteration later than " + minKey)
            val resultSet = byIteration(minKey)
            assert(resultSet.size == 1)
            resultSet.head
          } else {
            learningLog.logEvent(TableUpdateOperation(TableOperations.MergeTableEntries(groupedEntries)))
            val mergedReps: Set[SymbolicHeap] = (groupedEntries flatMap (_.reps)).toSet
            groupedEntries.head.copy(reps = mergedReps)
          }
        }
    }).toSeq
    obs.copy(entries = mergedEntries)
  }

}
