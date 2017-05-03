package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.seplog.inductive.{SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jens on 5/3/17.
  */
case class BreadthFirstUnfoldingsIterator(sid : SID, iteration: Int, continuation : Seq[SymbolicHeap], maxNumFV : Int, entailmentLog : EntailmentLearningLog) extends HarrshLogging {

  // TODO Return lazy iterator instead (and change processing functions accordingly)
  def continue : (Seq[SymbolicHeapPartition],BreadthFirstUnfoldingsIterator) = {

    logger.debug("Will unfold the following formulas:\n" + continuation.map(" - " + _).mkString("\n"))
    val nextUnfs = SIDUnfolding.unfoldOnce(sid, continuation)
    val (reducedUnfs, newContinuation) = nextUnfs.partition(_.predCalls.isEmpty)
    logger.debug("Reduced unfs for current iteration:\n" + reducedUnfs.map(" - " + _).mkString("\n"))
    logger.debug("Non-reduced unfs for next iteration:\n" + newContinuation.map(" - " + _).mkString("\n"))
    entailmentLog.logEvent(EntailmentLearningLog.IterationStats(iteration, reducedUnfs.size))

    val newPartitions : Seq[SymbolicHeapPartition] = for {
      nextUnf <- reducedUnfs
      //        printProgress("Processing Unfolding: " + nextUnf)
      simplifiedUnf = EqualityBasedSimplifications.fullEqualitySimplification(nextUnf)
      //        printProgress("After removing redundancies: " + simplifiedUnf)
      candidate <- partitions(simplifiedUnf, maxNumFV, entailmentLog)
    } yield candidate

    (newPartitions, copy(iteration = iteration + 1, continuation = newContinuation))
  }

  private def partitions(rsh: SymbolicHeap, maxNumFv : Int, entailmentLog : EntailmentLearningLog): Set[SymbolicHeapPartition] = {

    val spatialPartitions = Combinators.partitions(rsh.pointers.toSet)
    val purePartitions = Combinators.partitions(rsh.pure.toSet)
    entailmentLog.printProgress("Will look at " + spatialPartitions.size + " spatial and " + purePartitions.size + " pure combinations")

    for {
      (sigma1, sigma2) <- spatialPartitions
      // TODO Separate handling of emp?
      if !EntailmentAutomatonLearning.FindOnlyNonEmpty || sigma1.nonEmpty
      (pi1, pi2) <- purePartitions
      representative = SymbolicHeap(pi1.toSeq, sigma1.toSeq, Seq.empty)
      extension = SymbolicHeap(pi2.toSeq, sigma2.toSeq, Seq.empty)
      // FIXME Must consider all ways to name the new FVs in the representative...
      ecd = SymbolicHeapPartition(representative, extension)
      if ecd.repFV <= maxNumFv
    } yield ecd
  }

}
