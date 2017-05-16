package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.ObservationTable.TableEntry
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ReducedHeapEquivalence
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
    // FIXME Will the extensions really be identical or just equivalent?
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
            // FIXME Is this always a correct thing to do? (Should be in the 1-predicate case, but also in other cases?)
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
    * Searches the set of entries from the given iteration for representatives equivalent to the given heap
    * @param sh Heap to check equivalence against
    * @param it Only entries discovered in this iteration are searched
    * @return Some iteration with equivalent representative or None
    */
  def findEntryFromIterationWithEquivalentRepresentative(sh : SymbolicHeap, it : Int) : Option[TableEntry] = {
    val res = entries.find(entry => entry.discoveredInIteration == it && entry.containsEquivalentRepresentative(sh))
    res.foreach(entry => entailmentLearningLog.logEvent(EntailmentLearningLog.TableOperation(EntailmentLearningLog.TableOperation.FoundEquivalent(sh, entry))))
    res
  }

  /**
    * Searches for an entry whose associated equivalence class contains sh
    * @param sh Heap against which the entries/equivalence classes are checked
    * @return Some table entry representing the equivalence class of sh or None
    */
  def findEntryForEquivalenceClassOf(sh : SymbolicHeap) : Option[TableEntry] = {
    val res = entries.find {
      entry =>
        entailmentLearningLog.logEvent(EntailmentLearningLog.RedEntCheck(sh, entry.repSid.callToStartPred, EntailmentLearningLog.RedEntCheck.ReducibilityCheck(entry.reps)))
        entry.equivalenceClassContains(sh, reportProgress)
    }
    res.foreach(entry => {
      logger.debug("Can reduce " + sh + " to " + entry.reps)
      entailmentLearningLog.logEvent(EntailmentLearningLog.TableOperation(EntailmentLearningLog.TableOperation.FoundReduction(sh, entry)))
    })
    res
  }

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

  case class TableEntry(reps : Set[SymbolicHeap], exts : Set[SymbolicHeap], repParamInstantiation : Renaming, repSid : SID, isFinal : Boolean, discoveredInIteration : Int) {

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
      * @param sh Heap to check for equivalence
      * @return true iff there is an equivalent heap in the set of representatives
      */
    def containsEquivalentRepresentative(sh: SymbolicHeap) : Boolean = reps.exists(ReducedHeapEquivalence(_,sh))

    def equivalenceClassContains(sh : SymbolicHeap, reportProgress : Boolean = false) : Boolean = ReducedEntailment.checkSatisfiableRSHAgainstSID(sh, repSid.callToStartPred, repSid, reportProgress = reportProgress)
  }

}
