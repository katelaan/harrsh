package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment.SymbolicHeapPartition
import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{ConsistencyCheck, ReducedHeapEquivalence}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 5/23/17.
  */
case class TableEntry(reps : Set[SymbolicHeap], exts : Set[(SymbolicHeap,PredCall)], trackingInfo : TrackingInfo, /*repSid : SID,*/ discoveredInIteration : Int, introducedThroughClosure : Boolean) extends HarrshLogging {

  assert(reps.nonEmpty && exts.nonEmpty)
  assert(reps.map(_.numFV).size == 1)
  assert(exts.map(_._2.args.size).size == 1)
  assert(reps.head.numFV == exts.head._2.args.size)

  override def toString: String = "Entry(reps = " + reps.mkString("{", ", ", "}") + ", exts = " + exts.map(SymbolicHeapPartition.combinedString).mkString("{", ",", "}") + ", tracking = " + trackingInfo + ", isFinal = " + isFinal + ", i = " + discoveredInIteration + ")"

  lazy val isFinal : Boolean = {
    // Note: Dropped finality entailment check; instead mark as final iff the extension is emp. This has the result that final states might temporarily be marked as non-final, but by the time the algorithm terminates, we will always have encountered the emp extension
    //reducedEntailment(part.rep, sid.callToStartPred, sid, EntailmentLearningLog.RedEntCheck.FinalityCheck()),
    exts.exists(isTrivialExtension)
  }

  override def equals(o: scala.Any): Boolean = o match {
    case other : TableEntry => other.reps == reps && other.exts == exts
    case _ => false
  }

  override def hashCode(): Int = (reps,exts).hashCode()

  lazy val numFV : Int = {
    // This should be identical for all reps and exts (see also assertions above), so just picking an arbitrary one is fine
    reps.head.numFV
  }

  lazy val hasEmptyRepresentative : Boolean = reps.exists(!_.hasPointer)

  def asPartitionSequence: Seq[SymbolicHeapPartition] = (for {
    rep <- reps
    ext <- exts
  } yield SymbolicHeapPartition(rep, ext._1, ext._2)).toSeq

  def addExtension(ext : SymbolicHeap, extCall : PredCall) : TableEntry = copy(exts = exts + ((ext, extCall)))

  def addRepresentative(rep : SymbolicHeap) : TableEntry = copy(reps = reps + rep /*, repSid = RepresentativeSIDComputation.addBaseRule(repSid, rep)*/)

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
    * @param entailmentEngine Entailment checker to use
    * @return True iff rep in the class represented by this table entry
    */
  def equivalenceClassContains(rep : SymbolicHeap, sid : SID, entailmentEngine : ReducedEntailmentEngine) : Boolean = {
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
        val res = entailmentEngine.reducedEntailment(merged, sid.callToStartPred, sid, EntailmentLearningLog.RedEntCheck.ReducibilityCheck(rep))
        logger.debug("Checking reduced entailment of " + merged + " against original SID => " + res)
        res
      }
    }

    if (rep.numFV != numFV) {
      // If the representative and this class differ in the number of free variables, the rep can't be contained in this entry
      logger.debug("Representative and class differ in number of FVs => Don't belong to same class")
      false
    } else if (rep.freeVariableTrackingInfo != trackingInfo) {
      logger.debug("Representative and class differ in tracking info => Don't belong to same class")
      false
    } else {
      // The representative is assumed to be in the same class iff all combinations with extensions yield satisfiable
      // heaps that entail the SID
      exts forall doesCombinationEntailSID
    }
  }

  //TableEntry(reps :  exts :  /*repSid : SID,*/ isFinal : Boolean, discoveredInIteration : Int, introducedThroughClosure : Boolean) extends HarrshLogging {
  def renameRepParamsTo(renaming : Seq[Var]) : TableEntry = {
    val renamedReps = reps map (SymbolicHeap.renameFVs(_, renaming))
    val renamedExts = exts map (pair => (pair._1, ???))//PredCall(pair._2.name, renaming)))

    ??? //TableEntry(, exts map (SymbolicHeap.renameFVs(_, renaming)), isFinal)
  }

  //private def reorderCallArgs(renaming : Seq[Var])

  private def isTrivialExtension(ext : (SymbolicHeap,PredCall)) = {
    val argInts = ext._2.args map (_.getVarOrZero.toInt)
    // Triviality check is somewhat inefficient due to the creation of intermediate sequences
    ext._1.isEmpty && argInts.zip(1 to argInts.size).forall(pair => pair._1 == pair._2)
  }

}

object TableEntry {

  def entryWithMinimalExtensionSet(entries : Seq[TableEntry]) : TableEntry = {
    entries.tail.foldLeft(entries.head){
      case (fst, snd) =>
        if (hasMoreExtensions(fst,snd)) fst
        else if (hasMoreExtensions(snd,fst)) snd
        else throw new Throwable("Cannot resolve ambiguity in table entries")
    }
  }

  private def hasMoreExtensions(fst : TableEntry, snd : TableEntry) = {
    snd.exts subsetOf fst.exts
  }

  @deprecated("The learning process should only require access to the entry with weakest extensions, as this corresponds to the strongest representatives")
  def entryWithStrongestExtensions(entries : Seq[TableEntry]) : TableEntry = {
    entries.tail.foldLeft(entries.head){
      case (fst, snd) =>
        if (isStronger(fst,snd)) fst
        else if (isStronger(snd,fst)) snd
        else throw new Throwable("Cannot resolve ambiguity in table entries")
    }
  }

  def entryWithWeakestExtensions(entries : Seq[TableEntry]) : TableEntry = {
    entries.tail.foldLeft(entries.head){
      case (fst, snd) =>
        if (isStronger(fst,snd)) snd
        else if (isStronger(snd,fst)) fst
        else throw new Throwable("Cannot resolve ambiguity in table entries")
    }
  }

  private def isWeakerEntryIn(exts : Set[(Set[PointsTo], Set[PureAtom], PredCall)])(ext : (SymbolicHeap, PredCall)) : Boolean = {
    // FIXME This is a very naive check. Have to improve this as soon as we run into the ambiguity exception thrown above
    val (ptrSet, pureSet) = (ext._1.pointers.toSet, ext._1.pure.toSet)
    val matchingExt = exts.find(triple => triple._1 == ptrSet && triple._3 == ext._2)
    matchingExt.exists{
      _._2 subsetOf pureSet
    }
  }

  private def isStronger(fst : TableEntry, snd : TableEntry) = {
    val sndAsSets = snd.exts.map{
      case (ext,call) => (ext.pointers.toSet, ext.pure.toSet, call)
    }
    fst.exts forall isWeakerEntryIn(sndAsSets)
  }

}