package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{ConsistencyCheck, ReducedHeapEquivalence}
import at.forsyte.harrsh.seplog.Renaming
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

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

            entailmentLearningLog.logEvent(EntailmentLearningLog.MergeTableEntries(groupedEntries))
            // TODO The following might well be false, still need to figure out how to deal with different naming possibilites
            groupedEntries.foreach(entry => assert(entry.repParamInstantiation == groupedEntries.head.repParamInstantiation))
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
    res.foreach(entry => entailmentLearningLog.logEvent(EntailmentLearningLog.TableOperation(EntailmentLearningLog.TableOperation.FoundEquivalent(representative, entry))))
    res
  }

  /**
    * Searches for an entry whose associated equivalence class contains sh
    * @param sh Heap against which the entries/equivalence classes are checked
    * @return Some table entry representing the equivalence class of sh or None
    */
  private def findEntryForEquivalenceClassOf(sh : SymbolicHeap, checkOnlyFinalClasses : Boolean) : Option[TableEntry] = {
    logger.info("Trying to find equivalence class for " + sh)
    val entriesToCheck = if (checkOnlyFinalClasses) entries.filter(_.isFinal) else entries
    val res = entriesToCheck.find {
      entry =>
        // TODO Should actually log multiple entailment checks, as one is performed per representative and we're also using the original SID...
        entailmentLearningLog.logEvent(EntailmentLearningLog.RedEntCheck(sh, sid.callToStartPred, EntailmentLearningLog.RedEntCheck.ReducibilityCheck(entry.reps)))
        entry.equivalenceClassContains(sh, sid, reportProgress)
    }
    res.foreach(entry => {
      logger.debug("Can reduce " + sh + " to " + entry.reps)
      entailmentLearningLog.logEvent(EntailmentLearningLog.TableOperation(EntailmentLearningLog.TableOperation.FoundReduction(sh, entry)))
    })
    res
  }

  // FIXME This could be incorrect in some cases, because there is some nondeterminism left in the observation table! Idea: It might be possible to prioritize / only consider classes that do not lead to redundant pure constraints, but have to think this through
  // E.g. emp { x1 != x2 } is in the language and there are classes with extensions emp and emp : {x1 != x2}, then the heap emp : {x1 != x2} would actually be placed in both classes...
  def hasEntryForEquivalenceClassOf(sh : SymbolicHeap) : Boolean = findEntryForEquivalenceClassOf(sh, checkOnlyFinalClasses = false).nonEmpty

  def accepts(sh : SymbolicHeap, verbose : Boolean = false) : Boolean = {
    // Note: Checking only final classes is not only good for performance, but actually necessary at this points,
    // because the ObservationTable does *not* resolve all nondeterminism!
    val entry = findEntryForEquivalenceClassOf(sh, checkOnlyFinalClasses = true)
    if (verbose) entry match {
      case Some(x) => println(sh + " is in class of " + x + " generated by " + x.repSid)
      case None => println("No match for " + sh)
    }
    entry.exists(_.isFinal)
  }

  def rejects(sh : SymbolicHeap, verbose : Boolean = false) : Boolean = !accepts(sh, verbose)

  def addExtensionToEntry(entry : TableEntry, ext : SymbolicHeap) : ObservationTable = {
    entailmentLearningLog.logEvent(EntailmentLearningLog.TableOperation(EntailmentLearningLog.TableOperation.ExtendedEntry(entry, ext)))

    // TODO Maybe come up with a more efficient implementation of table entry update...
    val ix = entries.indexOf(entry)
    val newEntries = entries.updated(ix, entry.addExtension(ext))
    copy(entries = newEntries)
  }

  def addNewEntryForPartition(part : SymbolicHeapPartition, iteration : Int): ObservationTable = {
    entailmentLearningLog.logEvent(EntailmentLearningLog.TableOperation(EntailmentLearningLog.TableOperation.NewEntry(part)))
    entailmentLearningLog.printProgress("*** New ECD #" + (entries.size + 1) + ": " + part + " ***")
    copy(entries = entries :+ tableEntryFromPartition(part, iteration))
  }

  private def tableEntryFromPartition(part : SymbolicHeapPartition, iteration : Int) : TableEntry = {
    entailmentLearningLog.logEvent(EntailmentLearningLog.RedEntCheck(part.rep, sid.callToStartPred, EntailmentLearningLog.RedEntCheck.FinalityCheck()))

    TableEntry(
      Set(part.rep),
      Set(part.ext),
      part.repParamInstantiation,
      RepresentativeSIDComputation.adaptSIDToRepresentative(sid, part.rep),
      ReducedEntailment.checkSatisfiableRSHAgainstSID(part.rep, sid.callToStartPred, sid, entailmentLearningLog.reportProgress && EntailmentAutomatonLearning.ReportMCProgress),
      iteration)
  }

}

object ObservationTable {

  def empty(sid : SID, entailmentLearningLog: EntailmentLearningLog) : ObservationTable = ObservationTable(sid, Seq.empty, entailmentLearningLog)

  case class TableEntry(reps : Set[SymbolicHeap], exts : Set[SymbolicHeap], repParamInstantiation : Renaming, repSid : SID, isFinal : Boolean, discoveredInIteration : Int) extends HarrshLogging {

    override def toString: String = "Entry(reps = " + reps.mkString("{", ", ", "}") + ", exts = " + exts.mkString("{", ",", "}") + ", renaming = " + repParamInstantiation + ", isFinal = " + isFinal + ", i = " + discoveredInIteration + ")"

    override def equals(o: scala.Any): Boolean = o match {
      case other : TableEntry => other.reps == reps && other.exts == exts
      case _ => false
    }

    override def hashCode(): Int = (reps,exts).hashCode()

    def asPartitionSequence: Seq[SymbolicHeapPartition] = (for {
      rep <- reps
      ext <- exts
    } yield SymbolicHeapPartition(rep, ext, repParamInstantiation)).toSeq

    def addExtension(ext : SymbolicHeap) : TableEntry = copy(exts = exts + ext)

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

      logger.info("Checking inclusion of " + rep + " in " + this)

      exts.forall{
        ext =>
          val merged = SymbolicHeap.mergeHeaps(rep.renameVars(repParamInstantiation), ext, repParamInstantiation.codomain)
          if (!ConsistencyCheck.isConsistent(merged)) {
            logger.info("Not in class, because inconsistent: " + merged)
            false
          } else {
            val res = ReducedEntailment.checkSatisfiableRSHAgainstSID(merged, sid.callToStartPred, sid, reportProgress = reportProgress)
            logger.info("Checking reduced entailment of " + merged + " against original SID => " + res)
            res
          }
      }
    }
  }

}
