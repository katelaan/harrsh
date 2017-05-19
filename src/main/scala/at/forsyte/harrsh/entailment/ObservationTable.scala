package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog.{TableLookupOperation, TableOperations, TableUpdateOperation}
import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{ConsistencyCheck, ReducedHeapEquivalence}
import at.forsyte.harrsh.seplog.Renaming
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID, SymbolicHeap}

/**
  * Created by jens on 4/25/17.
  */
case class ObservationTable private (sid : SID, entries : Seq[TableEntry], entailmentLearningLog: EntailmentLearningLog) extends HarrshLogging {

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
      entry =>
        // TODO Should actually log multiple entailment checks, as one is performed per representative and we're also using the original SID...
        entailmentLearningLog.logEvent(EntailmentLearningLog.RedEntCheck(sh, sid.callToStartPred, EntailmentLearningLog.RedEntCheck.ReducibilityCheck(entry.reps)))
        entry.equivalenceClassContains(sh, sid, reportProgress)
    }
    res.foreach(entry => {
      logger.debug("Can reduce " + sh + " to " + entry.reps)
      entailmentLearningLog.logEvent(TableLookupOperation(TableOperations.FoundReduction(sh, entry)))
    })
    res
  }

  // FIXME This could be incorrect in some cases, because there is some nondeterminism left in the observation table! Idea: It might be possible to prioritize / only consider classes that do not lead to redundant pure constraints, but have to think this through
  // E.g. emp { x1 != x2 } is in the language and there are classes with extensions emp and emp : {x1 != x2}, then the heap emp : {x1 != x2} would actually be placed in both classes...
  def hasEntryForEquivalenceClassFromPreviousIterations(sh : SymbolicHeap, currentIteration : Int) : Boolean = findEntryForEquivalenceClassOf(sh, _.discoveredInIteration < currentIteration).nonEmpty

  def getEntryForEquivalenceClassFromCurrentIteration(sh : SymbolicHeap, currentIteration : Int) : Option[TableEntry] = findEntryForEquivalenceClassOf(sh, _.discoveredInIteration == currentIteration)

  def accepts(sh : SymbolicHeap, verbose : Boolean = false) : Boolean = {
    // Note: Checking only final classes is not only good for performance, but actually necessary at this points,
    // because the ObservationTable does *not* resolve all nondeterminism!
    val entry = findEntryForEquivalenceClassOf(sh, (_.isFinal))
    if (verbose) entry match {
      case Some(x) => println(sh + " is in class of " + x + " generated by " + x.repSid)
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
    entailmentLearningLog.logEvent(TableUpdateOperation(EntailmentLearningLog.TableOperations.NewEntry(part)))
    entailmentLearningLog.printProgress("*** New table entry #" + (entries.size + 1) + ": " + part + " ***")
    copy(entries = entries :+ tableEntryFromPartition(part, iteration))
  }

  private def tableEntryFromPartition(part : SymbolicHeapPartition, iteration : Int) : TableEntry = {
    entailmentLearningLog.logEvent(EntailmentLearningLog.RedEntCheck(part.rep, sid.callToStartPred, EntailmentLearningLog.RedEntCheck.FinalityCheck()))

    TableEntry(
      Set(part.rep),
      Set((part.ext,part.extPredCall)),
      RepresentativeSIDComputation.adaptSIDToRepresentative(sid, part.rep),
      // TODO It should be sufficient to just check for emp in the extension set, but then we might have to update "finality" later, because it is possible that we first discover rep with a nonempty extension
      ReducedEntailment.checkSatisfiableRSHAgainstSID(part.rep, sid.callToStartPred, sid, reportProgress),
      iteration)
  }

}

object ObservationTable {

  def empty(sid : SID, entailmentLearningLog: EntailmentLearningLog) : ObservationTable = ObservationTable(sid, Seq.empty, entailmentLearningLog)

  // TODO Maybe don't store extensions and their predicate calls separately? This would also remove issues with having to keep gaps in bound vars... (Would then have to change the SymbolicHeapPartition class as well)
  case class TableEntry(reps : Set[SymbolicHeap], exts : Set[(SymbolicHeap,PredCall)], repSid : SID, isFinal : Boolean, discoveredInIteration : Int) extends HarrshLogging {

    assert(reps.nonEmpty && exts.nonEmpty)
    assert(reps.map(_.numFV).size == 1)
    assert(exts.map(_._2.args.size).size == 1)
    assert(reps.head.numFV == exts.head._2.args.size)

    override def toString: String = "Entry(reps = " + reps.mkString("{", ", ", "}") + ", exts = " + exts.map(SymbolicHeapPartition.combinedString).mkString("{", ",", "}") + ", isFinal = " + isFinal + ", i = " + discoveredInIteration + ")"

    override def equals(o: scala.Any): Boolean = o match {
      case other : TableEntry => other.reps == reps && other.exts == exts
      case _ => false
    }

    override def hashCode(): Int = (reps,exts).hashCode()

    lazy val numFV : Int = {
      // This should be identical for all reps and exts (see also assertions above), so just picking an arbitrary one is fine
      reps.head.numFV
    }

    def asPartitionSequence: Seq[SymbolicHeapPartition] = (for {
      rep <- reps
      ext <- exts
    } yield SymbolicHeapPartition(rep, ext._1, ext._2)).toSeq

    def addExtension(ext : SymbolicHeap, extCall : PredCall) : TableEntry = copy(exts = exts + ((ext, extCall)))

    def addRepresentative(rep : SymbolicHeap) : TableEntry = copy(reps = reps + rep, repSid = RepresentativeSIDComputation.addBaseRule(repSid, rep))

    /**
      * Does this entry's set of representatives contain a heap that is equivalent to sh?
      * @param rep Heap to check for equivalence
      * @return true iff there is an equivalent heap in the set of representatives
      */
    def containsEquivalentRepresentative(rep: SymbolicHeap) : Boolean = reps.exists(ReducedHeapEquivalence(_,rep))

    /**
      * Does this table contain the given representative if interpreted as sid-equivalence class,
      * i.e. is the given representative compatible with all extensions of this table entry?
      * @param rep Representative whose equivalence we want to check
      * @param sid The SID we want to learn, i.e., the basis of the equivalence relation
      * @param reportProgress Should reduced-entailment progress be reported?
      * @return True iff rep in the class represented by this table entry
      */
    def equivalenceClassContains(rep : SymbolicHeap, sid : SID, reportProgress : Boolean = false) : Boolean = {
      // Note: The entailment check does *not* work, that's too weak: E.g. x1 -> x2 : { x1 != x2 } ENTAILS x1 -> x2,
      // so the following check would always place the former into the class of the latter!
      // ReducedEntailment.checkSatisfiableRSHAgainstSID(sh, repSid.callToStartPred, repSid, reportProgress = reportProgress)
      // That's why we really have to do the check for all extensions against the original SID!

      logger.debug("Checking inclusion of " + rep + " in " + this)

      def doesCombinationEntailSID(ext : (SymbolicHeap,PredCall)) : Boolean = {
        val merged = SymbolicHeapPartition(rep, ext._1, ext._2).recombined
        if (!ConsistencyCheck.isConsistent(merged)) {
          logger.debug("Not in class, because inconsistent: " + merged)
          false
        } else {
          val res = ReducedEntailment.checkSatisfiableRSHAgainstSID(merged, sid.callToStartPred, sid, reportProgress = reportProgress)
          logger.debug("Checking reduced entailment of " + merged + " against original SID => " + res)
          res
        }
      }

      if (rep.numFV != numFV) {
        // If the representative and this class differ in the number of free variables, the rep can't be contained in this entry
        logger.debug("Representative and class differ in number of FVs => Don't belong to same class")
        false
      } else {
        // The representative is assumed to be in the same class iff all combinations with extensions yield satisfiable
        // heaps that entail the SID
        exts forall doesCombinationEntailSID
      }
    }
  }

}
