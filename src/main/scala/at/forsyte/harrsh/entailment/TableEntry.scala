package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{ConsistencyCheck, ReducedHeapEquivalence}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID, SymbolicHeap}

/**
  * Created by jkatelaa on 5/23/17.
  */
// TODO Maybe don't store extensions and their predicate calls separately?
case class TableEntry(reps : Set[SymbolicHeap], exts : Set[(SymbolicHeap,PredCall)], /*repSid : SID,*/ isFinal : Boolean, discoveredInIteration : Int, introducedThroughClosure : Boolean) extends HarrshLogging {

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
    * @param reportProgress Should reduced-entailment progress be reported?
    * @return True iff rep in the class represented by this table entry
    */
  def equivalenceClassContains(rep : SymbolicHeap, sid : SID, reportProgress : Boolean, learningLog : EntailmentLearningLog) : Boolean = {
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
        val res = EntailmentAutomatonLearning.reducedEntailmentWithLogging(merged, sid.callToStartPred, sid, EntailmentLearningLog.RedEntCheck.ReducibilityCheck(rep), learningLog, reportProgress)
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

  def renameRepParamsTo(renaming : Seq[Var]) : TableEntry = ???

}
