package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.seplog.inductive.{SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jens on 3/6/17.
  */
object GenerateEntailmentAutomata extends HarrshLogging {

  /**
    * Maximum number of iterations of the ECD computation
    */
  val DebugLimit = 3 //Integer.MAX_VALUE
  /**
    * Remove redundant bound variables in ECDs?
    */
  val CleanUpSymbolicHeaps = true

  val EnableDetailedLog = true
  val ReportMCProgress = false

  val emptyLog = if (EnableDetailedLog) ECDComputationLog.completeLog else ECDComputationLog.dummyLog

  def apply(maxNumFV : Int, sid : SID, reportProgress : Boolean = false) : EntailmentHeapAutomaton = {

    val ecds = new ECDComputation(sid, maxNumFV, reportProgress = reportProgress).run()
    if (reportProgress) println("Finished ECD computation. Found " + ecds.size + " classes.")

    // TODO Symmetry, null parameters etc.

    // TODO Should of course do this on the fly while computing the ECDs...
    val stateDescriptors = for {
      ecd <- ecds
      entailsP = ReducedEntailment.checkSatisfiableRSHAgainstSID(ecd.rep, sid.callToStartPred, sid)
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

    val FindOnlyNonEmpty = true // Only generate non-empty left-hand sides. Still have to figure out if this is the right approach

    def run(): Seq[SymbolicHeapPartition] = {
      val (ecds,log) = ecdIteration(1, Seq.empty, Seq(sid.callToStartPred), emptyLog.setEntailmentRightHandSide(sid.callToStartPred))
      if (reportProgress) {
        println(log)
        println(log.statistics)
      }
      ecds
    }

    private def ecdIteration(i: Int, ecdPrev: Seq[SymbolicHeapPartition], partialUnfoldings: Seq[SymbolicHeap], oldLog : ECDComputationLog): (Seq[SymbolicHeapPartition],ECDComputationLog) = {

      def printProgress(msg: String): Unit = if (reportProgress) println("Iteration " + i + ": " + msg)

      val log = oldLog.incIteration

      printProgress("Starting new iteration")
      logger.debug("ECDs so far: " + ecdPrev.map(" - " + _).mkString("\n"))
      printProgress(log.statistics)
      printProgress("Will unfold the following formulas:\n" + partialUnfoldings.map(" - " + _).mkString("\n"))

      val nextUnfs = SIDUnfolding.unfoldOnce(sid, partialUnfoldings)
      val (reducedUnfs, newPartialUnfs) = nextUnfs.partition(_.predCalls.isEmpty)
      logger.debug("Reduced unfs for current iteration:\n" + reducedUnfs.map(" - " + _).mkString("\n"))
      logger.debug("Non-reduced unfs for next iteration:\n" + newPartialUnfs.map(" - " + _).mkString("\n"))

      printProgress("Stats: ecds(" + (i-1) + ") = "+ ecdPrev.size + "; #part-unf(" + (i-1) + ") = " + partialUnfoldings.size + "; #red-unf(" + i + ") = " + reducedUnfs.size + "; #part-unf(" + i + ") = " + newPartialUnfs.size)

      val res@(ecdNew,newLog) = processUnfoldings(reducedUnfs, ecdPrev, printProgress, log)

      if (ecdPrev.nonEmpty && ecdNew.size == ecdPrev.size) {
        // If we've already found at least one ECD, but now don't find a new one, we terminate
        val termMsg = "ECD computation reached fixed point"
        logger.debug(termMsg)
        printProgress(termMsg)
        res
      } else {
        if (i < DebugLimit) {
          // Found at least one new ECD => recurse
          logger.debug("Iteration " + i + ": Found " + (ecdNew.size-ecdPrev.size) + " new ECDs")
          ecdIteration(i + 1, ecdNew, newPartialUnfs, newLog)
        } else {
          logger.debug("Debug limit => Aborting ECD computation")
          res
        }

      }
    }

    @tailrec private def processUnfoldings(reducedUnfs : Seq[SymbolicHeap], ecdAcc : Seq[SymbolicHeapPartition], printProgress : String => Unit, log : ECDComputationLog) : (Seq[SymbolicHeapPartition],ECDComputationLog) = {
      if (reducedUnfs.isEmpty) (ecdAcc,log) else {
        printProgress(log.toString)
        printProgress(log.statistics)

        val nextUnf = reducedUnfs.head
        printProgress("Processing Unfolding: " + nextUnf)
        val simplifiedUnf = EqualityBasedSimplifications.fullEqualitySimplification(nextUnf)
        printProgress("After removing redundancies: " + simplifiedUnf)

        val newLog = log.logCurrentUnfolding(simplifiedUnf)
        newLog.incUnfCounter

        val candidates = partitions(simplifiedUnf, printProgress)
        printProgress("Partitions with " + (if (FindOnlyNonEmpty) "(non-empty)" else "(possibly empty)") + " left part to consider: " + candidates.size)
        val (ecdAccwithEcdsForUnfolding, newLog2) = processPartitions(candidates, ecdAcc, printProgress, newLog)
        processUnfoldings(reducedUnfs.tail, ecdAccwithEcdsForUnfolding, printProgress, newLog2)
      }
    }

    @tailrec private def processPartitions(candidates : Set[SymbolicHeapPartition], ecdAcc : Seq[SymbolicHeapPartition], printProgress : String => Unit, log : ECDComputationLog) : (Seq[SymbolicHeapPartition], ECDComputationLog) = {
      if (candidates.isEmpty) (ecdAcc,log) else {
        val ecd = candidates.head
        logger.debug("Processing Partition: " + ecd)
        log.incPartCounter
        val (newAcc,newLog) = if (!ecdAcc.contains(ecd)) {
          val (res, newLog) = isNew(ecdAcc, ecd, printProgress, log)
          if (res) {
            val cleanedECD = if (CleanUpSymbolicHeaps) ecd.simplify else ecd
            printProgress("*** New ECD #" + (ecdAcc.size + 1) + ": " + cleanedECD + " ***")
            (ecdAcc :+ cleanedECD, newLog.logNewECD(ecdAcc.size + 1, cleanedECD))
          } else {
            logger.debug("=> " + ecd + " assumed equal to previous ECD.")
            (ecdAcc, log)
          }
        } else {
          logger.debug("=> " + ecd + " assumed equal to previous ECD.")
          (ecdAcc,log)
        }

        processPartitions(candidates.tail, newAcc, printProgress, newLog)
      }
    }

    @tailrec private def isNew(oldECDs: Seq[SymbolicHeapPartition], candidate: SymbolicHeapPartition, printProgress : String => Unit, log : ECDComputationLog): (Boolean,ECDComputationLog) = if (oldECDs.isEmpty) {
      (true,log)
    } else {
      val (hd, tl) = (oldECDs.head, oldECDs.tail)
      logger.debug("Comparing against " + hd)
      val (notNew,newLog) = areBiExtensible(hd, candidate, printProgress, log)
      if (notNew) (false,newLog) else isNew(tl, candidate, printProgress, newLog)
    }

    private def areBiExtensible(fst: SymbolicHeapPartition, snd: SymbolicHeapPartition, printProgress : String => Unit, log : ECDComputationLog): (Boolean,ECDComputationLog) = {
      if (fst.repFV != snd.repFV) {
        logger.debug("Different number of FV => Not combinable")
        (false,log)
      }
      else {
        val (fstExt, sndExt) = fst.combine(snd)
        logger.debug("Checking 1st extension (" + fst.rep + ") * (" + snd.ext + "):\n    " + fstExt + " |?= " + sid.callToStartPred)
        val fstRes = reducedEntailment(fstExt, sid.callToStartPred)
        log.incRedEntCounter
        if (fstRes) {
          logger.debug("Checking 2nd extension (" + snd.rep + ") * (" + fst.ext + "):\n    " + sndExt + " |?= " + sid.callToStartPred)
          val res = reducedEntailment(sndExt, sid.callToStartPred)
          log.incRedEntCounter
          (res,log.logEntailmentCheck(snd,fstExt,fstRes=true,Some(sndExt),Some(res)))
        } else {
          logger.debug("1st entailment false => return false")
          (false,log.logEntailmentCheck(snd,fstExt,fstRes=false,None,None))
        }
      }
    }

    private def partitions(rsh: SymbolicHeap, printProgress : String => Unit): Set[SymbolicHeapPartition] = {

      val spatialPowerSet = Combinators.powerSet(rsh.pointers.toSet)
      val purePowerSet = Combinators.powerSet(rsh.pure.toSet)
      printProgress("Will look at " + spatialPowerSet.size + " spatial and " + purePowerSet.size + " pure combinations")

      for {
        sigma1 <- spatialPowerSet
        // TODO Separate handling of emp?
        if !FindOnlyNonEmpty || sigma1.nonEmpty
        pi1 <- purePowerSet
        // TODO Powerset computation that returns subsets together with their complements
        sigma2 = rsh.pointers.toSet -- sigma1
        pi2 = rsh.pure.toSet -- pi1
        representative = SymbolicHeap(pi1.toSeq, sigma1.toSeq, Seq.empty)
        extension = SymbolicHeap(pi2.toSeq, sigma2.toSeq, Seq.empty)
        // FIXME Must consider all ways to name the new FVs in the representative...
        ecd = {
          val ecd = SymbolicHeapPartition(representative, extension)
          print(".")
          ecd
        }
        if ecd.repFV <= maxNumFv
      } yield ecd
    }

    private def reducedEntailment(lhs: SymbolicHeap, rhs: SymbolicHeap) : Boolean = ReducedEntailment.checkSatisfiableRSHAgainstSID(lhs, rhs, sid, reportProgress && ReportMCProgress)

  }

}
