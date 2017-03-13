package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jens on 3/6/17.
  */
object GenerateEntailmentAutomata extends HarrshLogging {

  def apply(maxNumFV : Int, sid : SID, reportProgress : Boolean = false) : EntailmentHeapAutomaton = {

    val ecds = new ECDComputation(sid, maxNumFV, reportProgress = reportProgress).run()
    if (reportProgress) println("Finished ECD computation. Found " + ecds.size + " classes.")

    // TODO Symmetry, null parameters etc.

    // TODO Should of course do this on the fly while computing the ECDs...
    val stateDescriptors = for {
      (r,e) <- ecds
      entailsP = GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(r, sid.callToStartPred, sid)
    } yield (r,e,entailsP)

    for {
      (r,e,f) <- stateDescriptors
    } {
      println("representative: " + r + " extension: " + e + " final: " + f)
    }

    new EntailmentHeapAutomaton(maxNumFV, stateDescriptors)
  }

  /**
    * Equivalence class descriptors
    */
  type ECD = (SymbolicHeap, SymbolicHeap)

  private class ECDComputation(sid : SID, maxNumFv : Int, reportProgress : Boolean) {

    def run(): Seq[ECD] = {
      ecdIteration(1, Seq.empty, Seq(sid.callToStartPred))
    }

    private def ecdIteration(i: Int, ecdPrev: Seq[ECD], partialUnfoldings: Seq[SymbolicHeap]): Seq[ECD] = {

      def printProgress(msg: String): Unit = if (reportProgress) println("Iteration " + i + ": " + msg)

      printProgress("Starting new iteration")

      val nextUnfs = SIDUnfolding.unfoldOnce(sid, partialUnfoldings)
      val (reducedUnfs, newPartialUnfs) = nextUnfs.partition(_.predCalls.isEmpty)

      printProgress("Old ECDs: " + ecdPrev.size + "; Partial lvl. i-1 unfs.: " + partialUnfoldings.size + "; Lvl. i reduced/non-reduced unfs.: " + reducedUnfs.size + "/" + newPartialUnfs.size)

      val ecdNew = for {
        unf <- reducedUnfs
        candidate <- partitions(unf, printProgress)
        if ecdPrev.isEmpty || isNew(ecdPrev, candidate)
      } yield {
        printProgress("Found " + candidate)
        candidate
      }

      if (ecdNew.isEmpty) {
        val termMsg = "ECD computation reached fixed point";
        logger.debug(termMsg);
        printProgress(termMsg)
        ecdPrev
      } else {
        // Found at least one new ECD => recurse
        printProgress("Found " + ecdNew.size + " new ECDs");
        logger.debug("Iteration " + i + ": Found " + ecdNew.size + " new ECDs")
        ecdIteration(i + 1, ecdPrev ++ ecdNew, newPartialUnfs)
      }
    }

    private def isNew(ecds: Seq[ECD], candidate: ECD): Boolean = if (ecds.isEmpty) {
      false
    } else {
      val (hd, tl) = (ecds.head, ecds.tail)
      val notNew = areBiExtensible(hd, candidate)
      if (notNew) isNew(tl, candidate) else true
    }

    private def areBiExtensible(fst: ECD, snd: ECD): Boolean = {
      val fstExt = SymbolicHeap.combineHeaps(fst._1, snd._2)
      val sndExt = SymbolicHeap.combineHeaps(snd._1, fst._2)
      reducedEntailment(fstExt, sid.callToStartPred) && reducedEntailment(sndExt, sid.callToStartPred)
    }

    private def partitions(rsh: SymbolicHeap, printProgress : String => Unit): Set[ECD] = {
      val res = for {
        sigma1 <- Combinators.powerSet(rsh.spatial.toSet)
        if !sigma1.isEmpty
        pi1 <- Combinators.powerSet(rsh.pure.toSet)
        // TODO Powerset computation that returns subsets together with their complements
        sigma2 = rsh.spatial.toSet -- sigma1
        pi2 = rsh.pure.toSet -- pi1
        representative = SymbolicHeap(pi1.toSeq, sigma1.toSeq, Seq.empty)
        extension = SymbolicHeap(pi2.toSeq, sigma2.toSeq, Seq.empty)
        // FIXME Must consider all ways to name the new FVs in the representative...
        repWithExtPoints = unbindShared(representative, extension)
        if repWithExtPoints.numFV <= maxNumFv
        extWithExtPoints = unbindShared(extension, representative)
      } yield (repWithExtPoints, extWithExtPoints)
      printProgress("Partitions to consider: " + res.size)
      res
    }

    private def unbindShared(rshToModify : SymbolicHeap, sharedWith : SymbolicHeap) : SymbolicHeap = {
      def unbindAll(vars : Seq[Var], sh : SymbolicHeap) : SymbolicHeap = if (vars.isEmpty) sh else {
        val nextSH = sh.instantiateBoundVar(vars.head, sh.numFV+1)
        unbindAll(vars.tail, nextSH)
      }

      val sharedVars = rshToModify.boundVars.toSet intersect sharedWith.boundVars.toSet
      // FIXME Actually should consider all ways to order the FVs? See also the comment above
      unbindAll(sharedVars.toSeq, rshToModify)
    }

    private def reducedEntailment(lhs: SymbolicHeap, rhs: SymbolicHeap) : Boolean = GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhs, rhs, sid)

  }

}
