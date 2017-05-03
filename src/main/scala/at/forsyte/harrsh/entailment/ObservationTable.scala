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

  override def toString: String = entries.map("  " + _).mkString("ObservationTable(\n", "\n", "\n)")

  override def equals(o: scala.Any): Boolean = o match {
    case other : ObservationTable => other.entries == entries
    case _ => false
  }

  override def hashCode(): Int = entries.hashCode()

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

//  def updateTableWithPartition(part : SymbolicHeapPartition, iteration : Int) : ObservationTable = {
//
//    logger.debug("Processing Partition: " + part)
//    entailmentLearningLog.logEvent(EntailmentLearningLog.ProcessPartition(part))
//
//    findEntry(part.rep) match {
//      case Some(entry) =>
//        // Check if it was discovered in this or in one of the previous iterations
//        if (entry.discoveredInIteration < iteration) {
//          // Entry is old => entry is sealed => no update
//          logger.debug("Have seen " + part.rep + " in previous iteration, discarding " + part)
//          this
//        } else {
//          // New extension for the same representative, extend entry
//          logger.debug("Will add new extension " + part.ext + " to table entry for " + part.rep)
//          // FIXME Is this actually true? Or do we have to do some renaming instead?
//          assert(part.repParamInstantiation == entry.repParamInstantiation)
//          updateEntry(entry, part.ext)
//        }
//      case None =>
//        // This exact representative is *not* in the table, check if it is the extension of one in the table
//        core(part.rep) match {
//          case Some(reducedEntry) =>
//            // Have already dealt with the same partition in the previous iteration, discard
//            logger.debug("Can reduce " + part.rep + " to " + reducedEntry.rep + ", discarding " + part)
//            this
//          case None =>
//            // The representative is genuinely new. We will keep it for the time being.
//            // It might have to be merged with some other entry at the end of the iteration, though
//            val cleanedECD = if (EntailmentAutomatonLearning.CleanUpSymbolicHeaps) part.simplify else part
//            entailmentLearningLog.printProgress("*** New ECD #" + (entries.size + 1) + ": " + cleanedECD + " ***")
//            copy(entries = entries :+ tableEntryFromPartition(cleanedECD))
//        }
//    }
//
//  }

  def findEntryWithEquivalentRepresentative(sh : SymbolicHeap) : Option[TableEntry] = entries.find{
    entry => entry.reps.exists(ReducedHeapEquivalence(_,sh))
  }

  def findReducibleEntry(sh : SymbolicHeap) : Option[TableEntry] = entries.find{
    entry =>
      entailmentLearningLog.logEvent(EntailmentLearningLog.RedEntCheck(sh, entry.repSid.callToStartPred, EntailmentLearningLog.RedEntCheck.ReducibilityCheck(entry.reps)))
      reducedEntailment(sh, entry.repSid.callToStartPred, entry.repSid)
  }

  def updateEntry(entry : TableEntry, ext : SymbolicHeap) : ObservationTable = {
    // TODO Maybe come up with a more efficient implementation of table entry update...
    val ix = entries.indexOf(entry)
    val newEntries = entries.updated(ix, entry.addExtension(ext))
    copy(entries = newEntries)
  }

  def addNewEntryForPartition(part : SymbolicHeapPartition, iteration : Int): ObservationTable = {
    entailmentLearningLog.printProgress("*** New ECD #" + (entries.size + 1) + ": " + part + " ***")
    copy(entries = entries :+ tableEntryFromPartition(part, iteration))
  }

  // TODO Is it correct that we can now drop the bi-extensibility check to determine whether classes are new?
//  @tailrec private def isNew(candidate: SymbolicHeapPartition): Boolean = if (entries.isEmpty) {
//    true
//  } else {
//    val (hd, tl) = (entries.head, entries.tail)
//    logger.debug("Comparing against " + hd)
//    // FIXME Take into account all, not just the head of the partition sequence
//    val (notNew,newLog) = areBiExtensible(hd.asPartitionSequence.head, candidate)
//    if (notNew) false else isNew(tl, candidate)
//  }
//
//  private def areBiExtensible(fst: SymbolicHeapPartition, snd: SymbolicHeapPartition): Boolean = {
//    if (fst.repFV != snd.repFV) {
//      logger.debug("Different number of FV => Not combinable")
//      false
//    }
//    else {
//      val (fstExt, sndExt) = fst.combine(snd)
//      logger.debug("Checking 1st extension (" + fst.rep + ") * (" + snd.ext + "):\n    " + fstExt + " |?= " + sid.callToStartPred)
//      val fstRes = reducedEntailment(fstExt, sid.callToStartPred)
////      log.incRedEntCounter
//      if (fstRes) {
//        logger.debug("Checking 2nd extension (" + snd.rep + ") * (" + fst.ext + "):\n    " + sndExt + " |?= " + sid.callToStartPred)
//        val res = reducedEntailment(sndExt, sid.callToStartPred)
////        log.incRedEntCounter
////        (res,log.logEntailmentCheck(snd,fstExt,fstRes=true,Some(sndExt),Some(res)))
//        res
//      } else {
//        logger.debug("1st entailment false => return false")
////        (false,log.logEntailmentCheck(snd,fstExt,fstRes=false,None,None))
//        false
//      }
//    }
//  }

  private def reducedEntailment(lhs: SymbolicHeap, rhs: SymbolicHeap, sid : SID) : Boolean = {
    GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhs, rhs, sid, entailmentLearningLog.reportProgress && EntailmentAutomatonLearning.ReportMCProgress)
  }

  private def tableEntryFromPartition(part : SymbolicHeapPartition, iteration : Int) : TableEntry = {
    entailmentLearningLog.logEvent(EntailmentLearningLog.RedEntCheck(part.rep, sid.callToStartPred, EntailmentLearningLog.RedEntCheck.FinalityCheck()))

    TableEntry(
      Set(part.rep),
      Set(part.ext),
      part.repParamInstantiation,
      RepresentativeSIDComputation.adaptSIDToRepresentative(sid, part.rep),
      reducedEntailment(part.rep, sid.callToStartPred, sid),
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
  }

}
