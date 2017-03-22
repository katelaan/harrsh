package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jens on 3/6/17.
  */
object GenerateEntailmentAutomata extends HarrshLogging {

  val DebugLimit = 10

  def apply(maxNumFV : Int, sid : SID, reportProgress : Boolean = false) : EntailmentHeapAutomaton = {

    val ecds = new ECDComputation(sid, maxNumFV, reportProgress = reportProgress).run()
    if (reportProgress) println("Finished ECD computation. Found " + ecds.size + " classes.")

    // TODO Symmetry, null parameters etc.

    // TODO Should of course do this on the fly while computing the ECDs...
    val stateDescriptors = for {
      ecd <- ecds
      entailsP = GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(ecd.rep, sid.callToStartPred, sid)
    } yield (ecd.rep, ecd.ext, entailsP)

    for {
      ((r,e,f),i) <- stateDescriptors.zipWithIndex
    } {
      println("ECD #" + (i+1) + ": representative: " + r + " @ " + r.numFV + "; extension: " + e + " @ " + e.numFV + "; final: " + f)
    }

    new EntailmentHeapAutomaton(maxNumFV, stateDescriptors)
  }

  private class ECDComputation(sid : SID, maxNumFv : Int, reportProgress : Boolean) {

    // TODO Pure formula treatment: Should probably look just at spatial part of partitions and keep track of pure part separately (tracking)? But then have to modify the reduced entailment check?

    def FindOnlyNonEmpty = true // Only generate non-empty left-hand sides. Still have to figure out if this is the right approach

    def run(): Seq[ECD] = {
      ecdIteration(1, Seq.empty, Seq(sid.callToStartPred))
    }

    private def ecdIteration(i: Int, ecdPrev: Seq[ECD], partialUnfoldings: Seq[SymbolicHeap]): Seq[ECD] = {

      def printProgress(msg: String): Unit = if (reportProgress) println("Iteration " + i + ": " + msg)

      printProgress("Starting new iteration; ECDs so far: " + ecdPrev.map(" - " + _).mkString("\n"))
      printProgress("Will unfold the following formulas:\n" + partialUnfoldings.map(" - " + _).mkString("\n"))

      val nextUnfs = SIDUnfolding.unfoldOnce(sid, partialUnfoldings)
      val (reducedUnfs, newPartialUnfs) = nextUnfs.partition(_.predCalls.isEmpty)
      printProgress("Reduced unfs for current iteration:\n" + reducedUnfs.map(" - " + _).mkString("\n"))
      printProgress("Non-reduced unfs for next iteration:\n" + newPartialUnfs.map(" - " + _).mkString("\n"))

      printProgress("Stats: ecds(" + (i-1) + ")="+ ecdPrev.size + "; part-unf(" + (i-1) + ")=" + partialUnfoldings.size + "; red-unf(" + i + ")=" + reducedUnfs.size + "; part-unf(" + i + ")=" + newPartialUnfs.size)

      val ecdNew = processUnfoldings(reducedUnfs, ecdPrev, printProgress)

      if (!ecdPrev.isEmpty && ecdNew.size == ecdPrev.size) {
        // If we've already found at least one ECD, but now don't find a new one, we terminate
        val termMsg = "ECD computation reached fixed point"
        logger.debug(termMsg);
        printProgress(termMsg)
        ecdNew
      } else {
        if (i < DebugLimit) {
          // Found at least one new ECD => recurse
          printProgress("Found " + (ecdNew.size-ecdPrev.size) + " new ECDs")
          logger.debug("Iteration " + i + ": Found " + (ecdNew.size-ecdPrev.size) + " new ECDs")
          ecdIteration(i + 1, ecdNew, newPartialUnfs)
        } else {
          printProgress("Debug limit => Aborting ECD computation")
          logger.debug("Debug limit => Aborting ECD computation")
          ecdNew
        }

      }
    }

    @tailrec private def processUnfoldings(reducedUnfs : Seq[SymbolicHeap], ecdAcc : Seq[ECD], printProgress : String => Unit) : Seq[ECD] = {
      if (reducedUnfs.isEmpty) ecdAcc else {
        printProgress("Processing Unfolding: " + reducedUnfs.head)
        val candidates = partitions(reducedUnfs.head)
        printProgress("Partitions with " + (if (FindOnlyNonEmpty) "(non-empty)" else "(possibly empty)") + " left part to consider: " + candidates.size)
        val ecdAccwithEcdsForUnfolding = processPartitions(candidates, ecdAcc, printProgress)
        processUnfoldings(reducedUnfs.tail, ecdAccwithEcdsForUnfolding, printProgress)
      }
    }

    @tailrec private def processPartitions(candidates : Set[ECD], ecdAcc : Seq[ECD], printProgress : String => Unit) : Seq[ECD] = {
      if (candidates.isEmpty) ecdAcc else {
        val ecd = candidates.head
        printProgress("Processing Partition: " + ecd)
        val newAcc = if (!ecdAcc.contains(ecd) && isNew(ecdAcc, ecd, printProgress)) {
          printProgress("*** New ECD #" + (ecdAcc.size+1) + ": " + ecd + " ***")
          ecdAcc :+ ecd
        } else {
          printProgress("=> " + ecd + " assumed equal to previous ECD.")
          ecdAcc
        }

        processPartitions(candidates.tail, newAcc, printProgress : String => Unit)
      }
    }

    @tailrec private def isNew(oldECDs: Seq[ECD], candidate: ECD, printProgress : String => Unit): Boolean = if (oldECDs.isEmpty) {
      true
    } else {
      val (hd, tl) = (oldECDs.head, oldECDs.tail)
      printProgress("Comparing against " + hd)
      val notNew = areBiExtensible(hd, candidate, printProgress)
      if (notNew) false else isNew(tl, candidate, printProgress)
    }

    private def areBiExtensible(fst: ECD, snd: ECD, printProgress : String => Unit): Boolean = {
      val (fstExt, sndExt) = fst.combine(snd)
      printProgress("Checking 1st extension (" + fst.rep + ") * (" + snd.ext + "):\n    " +  fstExt + " |?= " + sid.callToStartPred)
      val fstRes = reducedEntailment(fstExt, sid.callToStartPred)
      if (fstRes) {
        printProgress("Checking 2nd extension (" + snd.rep + ") * (" + fst.ext + "):\n    " +  sndExt + " |?= " + sid.callToStartPred)
        reducedEntailment(sndExt, sid.callToStartPred)
      } else {
        printProgress("1st entailment false => return false")
        false
      }
    }

    private def partitions(rsh: SymbolicHeap): Set[ECD] = {
      for {
        sigma1 <- Combinators.powerSet(rsh.pointers.toSet)
        // TODO Separate handling of emp?
        if FindOnlyNonEmpty && !sigma1.isEmpty
        pi1 <- Combinators.powerSet(rsh.pure.toSet)
        // TODO Powerset computation that returns subsets together with their complements
        sigma2 = rsh.pointers.toSet -- sigma1
        pi2 = rsh.pure.toSet -- pi1
        representative = SymbolicHeap(pi1.toSeq, sigma1.toSeq, Seq.empty)
        extension = SymbolicHeap(pi2.toSeq, sigma2.toSeq, Seq.empty)
        // FIXME Must consider all ways to name the new FVs in the representative...
        ecd = ECD(representative, extension)
        if ecd.repFV <= maxNumFv
      } yield ecd
    }

    private def reducedEntailment(lhs: SymbolicHeap, rhs: SymbolicHeap) : Boolean = GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhs, rhs, sid, reportProgress)

  }

}
