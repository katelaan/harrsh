package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentAutomatonLearning.LearningMode
import at.forsyte.harrsh.entailment.EntailmentLearningLog.{TableLookupOperation, TableOperations, TableUpdateOperation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID, SymbolicHeap}

/**
  * Created by jens on 4/25/17.
  */
case class ObservationTable private (learningMode : LearningMode, sid : SID, entries : Seq[TableEntry], entailmentLearningLog: EntailmentLearningLog) extends HarrshLogging {

  private val reportProgress: Boolean = entailmentLearningLog.reportProgress && EntailmentAutomatonLearning.ReportMCProgress

  override def toString: String = entries.map("  " + _).mkString("ObservationTable(\n", "\n", "\n)")

  override def equals(o: scala.Any): Boolean = {
    // Overriding equals to ignore the log
    o match {
      case other : ObservationTable => other.entries == entries
      case _ => false
    }
  }

  override def hashCode(): Int = {
    // Overriding equals to ignore the log
    entries.hashCode()
  }

  def numClasses : Int = entries.size

  def finalClasses = entries.filter(_.isFinal)

  def mergeDuplicateEntries : ObservationTable = {
    // Merge entries with identical sets of extensions
    // FIXME Will the extensions really be identical or just equivalent? Should come up with a counterexample where we need the latter!
    val grouped = entries.groupBy(_.exts)
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
            entailmentLearningLog.logEvent(TableUpdateOperation(TableOperations.MergeTableEntries(groupedEntries)))
            val mergedReps: Set[SymbolicHeap] = (groupedEntries flatMap (_.reps)).toSet
            groupedEntries.head.copy(reps = mergedReps)
          }
        }
    }).toSeq
    copy(entries = mergedEntries)
  }

  /**
    * Searches the set of entries for an entry from the given iteration whose set of representatives contains a
    * representative matching the given representative (i.e., an entry with a rep equivalent to the given heap)
    * @param representative Heap whose entry we try to find
    * @param iteration Only entries discovered in this iteration are searched
    * @return Some iteration with equivalent representative or None
    */
  def findMatchingEntryFromIteration(representative : SymbolicHeap, iteration : Int) : Option[TableEntry] = {
    val res = entries.find(entry => entry.discoveredInIteration == iteration && entry.containsEquivalentRepresentative(representative))
    res.foreach(entry => entailmentLearningLog.logEvent(TableLookupOperation(TableOperations.FoundEquivalent(representative, entry))))
    res
  }

  /**
    * Searches for an entry whose associated equivalence class contains sh
    * @param sh Heap against which the entries/equivalence classes are checked
    * @return Some table entry representing the equivalence class of sh or None
    */
  private def findEntryForEquivalenceClassOf(sh : SymbolicHeap, filterPredicate : TableEntry => Boolean) : Option[TableEntry] = {
    logger.debug("Trying to find equivalence class for " + sh)
    val entriesToCheck = entries filter filterPredicate
    val res = entriesToCheck.find {
      _.equivalenceClassContains(sh, sid, reportProgress, entailmentLearningLog)
    }
    res.foreach(entry => {
      logger.debug("Can reduce " + sh + " to " + entry.reps)
      entailmentLearningLog.logEvent(TableLookupOperation(TableOperations.FoundReduction(sh, entry)))
    })
    res
  }

  // FIXME This could be incorrect in some cases, because there is some nondeterminism left in the observation table! Idea: It might be possible to prioritize / only consider classes that do not lead to redundant pure constraints, but have to think this through
  // E.g. if emp { x1 != x2 } is in the language and there are classes with extensions emp and emp : {x1 != x2}, then the heap emp : {x1 != x2} would actually be placed in both classes...
  def hasMatchingEntryFromPreviousIterations(sh : SymbolicHeap, currentIteration : Int) : Boolean = findEntryForEquivalenceClassOf(sh, _.discoveredInIteration < currentIteration).nonEmpty

  /**
    * Returns some unifished entry containing rep (i.e., entry that can still be extended); an entry is unfinished if it is from the current iteration or if it was from the previous iteration and contains emp.
    * @param rep Representative whose entry we look for
    * @param currentIteration Current iteration
    * @return Some matching entry or nothing
    */
  def getUnfinishedMatchingEntry(rep : SymbolicHeap, currentIteration : Int) : Option[TableEntry] = {
    // If the learning mode closes under emp, i.e. treats emp like every other representative, no special treatment of emp reps is necesary;
    // In that case, only entries from the current iteration are treated as unfinished
    // On the other hand, if emp needs special treatment, we might split off its own equivalence class later; we therefore need to find another representative of the class, which might happen only in the iteration after emp is discovered
    // TODO Is this really the right criterion? What if we disocver both emp and another representative in the same iteration? (I think we still need to consider the next iteration, but am not 100% sure)
    def isUnfinished(entry : TableEntry) = entry.discoveredInIteration == currentIteration || (!learningMode.closedUnderEmp && entry.discoveredInIteration == currentIteration-1 && entry.hasEmptyRepresentative)
    findEntryForEquivalenceClassOf(rep, isUnfinished)
  }

  def getAllMatchingEntries(sh : SymbolicHeap) : Seq[TableEntry] = {
    // TODO Code duplication wrt findEntryForEquivalenceClassOf
    logger.debug("Finding all entries for " + sh)
    val res = entries filter {
      entry =>
        entry.equivalenceClassContains(sh, sid, reportProgress, entailmentLearningLog)
    }
    res.foreach(entry => {
      logger.debug("Can reduce " + sh + " to " + entry.reps)
      entailmentLearningLog.logEvent(TableLookupOperation(TableOperations.FoundReduction(sh, entry)))
    })
    res
  }

  def accepts(sh : SymbolicHeap, verbose : Boolean = false) : Boolean = {
    // Note: Checking only final classes is not only good for performance, but actually necessary at this points,
    // because the ObservationTable does *not* resolve all nondeterminism!
    val entry = findEntryForEquivalenceClassOf(sh, _.isFinal)
    if (verbose) entry match {
      case Some(x) => println(sh + " is in class of " + x) // + " generated by " + x.repSid)
      case None => println("No match for " + sh)
    }
    entry.exists(_.isFinal)
  }

  def rejects(sh : SymbolicHeap, verbose : Boolean = false) : Boolean = !accepts(sh, verbose)

  def addRepresentativeToEntry(entry : TableEntry, rep : SymbolicHeap) : ObservationTable = {
    entailmentLearningLog.logEvent(TableUpdateOperation(TableOperations.EnlargedEntry(entry, rep, isNewRepresentative = true)))

    // TODO Maybe come up with a more efficient implementation of table entry update... Also code duplication
    val ix = entries.indexOf(entry)
    val newEntries = entries.updated(ix, entry.addRepresentative(rep))
    copy(entries = newEntries)
  }

  def addExtensionToEntry(entry : TableEntry, ext : SymbolicHeap, extPredCall : PredCall) : ObservationTable = {
    entailmentLearningLog.logEvent(TableUpdateOperation(TableOperations.EnlargedEntry(entry, ext, isNewRepresentative = false)))

    // TODO Maybe come up with a more efficient implementation of table entry update... Also code duplication
    val ix = entries.indexOf(entry)
    val newEntries = entries.updated(ix, entry.addExtension(ext, extPredCall))
    copy(entries = newEntries)
  }

  def splitEntry(entry: TableEntry, compatibleWithNewExtension: Set[SymbolicHeap], incompatibleWithNewExtension: Set[SymbolicHeap], newExtension: SymbolicHeap, newExtensionCall : PredCall): ObservationTable = {
    entailmentLearningLog.logEvent(TableUpdateOperation(TableOperations.SplitTableEntry(compatibleWithNewExtension, incompatibleWithNewExtension, newExtension)))

    // TODO Same concern about efficient table entry as above...
    val entriesWithoutOldEntry = entries.filterNot(_ == entry)
    val compatibleEntry = entry.copy(reps = compatibleWithNewExtension).addExtension(newExtension, newExtensionCall)
    val incompatibleEntry = entry.copy(reps = incompatibleWithNewExtension)

    copy(entries = entriesWithoutOldEntry :+ compatibleEntry :+ incompatibleEntry)
  }

  def addNewEntryForPartition(part : SymbolicHeapPartition, iteration : Int): ObservationTable = {
    entailmentLearningLog.logEvent(TableUpdateOperation(EntailmentLearningLog.TableOperations.NewEntry(entries.size+1, part)))
    copy(entries = entries :+ tableEntryFromPartition(part, iteration))
  }

  private def tableEntryFromPartition(part : SymbolicHeapPartition, iteration : Int) : TableEntry = {
    TableEntry(
      Set(part.rep),
      Set((part.ext,part.extPredCall)),
      //RepresentativeSIDComputation.adaptSIDToRepresentative(sid, part.rep),
      // TODO It should be sufficient to just check for emp in the extension set, but then we might have to update "finality" later, because it is possible that we first discover rep with a nonempty extension
      EntailmentAutomatonLearning.reducedEntailmentWithLogging(part.rep, sid.callToStartPred, sid, EntailmentLearningLog.RedEntCheck.FinalityCheck(), entailmentLearningLog, reportProgress),
      iteration,
      introducedThroughClosure = false)
  }

}

object ObservationTable {

  def empty(learningMode: LearningMode, sid : SID, entailmentLearningLog: EntailmentLearningLog) : ObservationTable = ObservationTable(learningMode, sid, Seq.empty, entailmentLearningLog)

}
