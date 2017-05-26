package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment.SymbolicHeapPartition
import at.forsyte.harrsh.main._
import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.seplog.inductive.{SID, SIDUnfolding, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jens on 5/3/17.
  */
case class BreadthFirstUnfoldingsIterator(sid : SID, iteration: Int, continuation : Seq[SymbolicHeap], maxNumFV : Int, entailmentLog : EntailmentLearningLog, config : PartitionFilter with SymmetryHandler) extends HarrshLogging {

  // TODO Return lazy iterator instead (and change processing functions accordingly)
  def continue : (Seq[SymbolicHeapPartition],BreadthFirstUnfoldingsIterator) = {

    logger.debug("Will unfold the following formulas:\n" + continuation.map(" - " + _).mkString("\n"))
    val nextUnfs = SIDUnfolding.unfoldOnce(sid, continuation)
    val (reducedUnfsCandidates, newContinuation) = nextUnfs.partition(_.isReduced)
    val reducedUnfs = config.symmetryInProcessing(reducedUnfsCandidates)
    logger.debug("Reduced unfs for current iteration:\n" + reducedUnfs.map(" - " + _).mkString("\n"))
    logger.debug("Non-reduced unfs for next iteration:\n" + newContinuation.map(" - " + _).mkString("\n"))
    entailmentLog.logEvent(EntailmentLearningLog.IterationStats(iteration, reducedUnfs.size))

    val newPartitions : Seq[SymbolicHeapPartition] = for {
      nextUnf <- reducedUnfs
      simplifiedUnf = EqualityBasedSimplifications.fullEqualitySimplification(nextUnf)
      candidate <- partitions(simplifiedUnf, maxNumFV, entailmentLog)
    } yield candidate

    (newPartitions, copy(iteration = iteration + 1, continuation = newContinuation))
  }

  private def partitions(rsh: SymbolicHeap, maxNumFv : Int, entailmentLog : EntailmentLearningLog): Set[SymbolicHeapPartition] = {

    val spatialPartitions = Combinators.partitions(rsh.pointers.toSet)
    val purePartitions = Combinators.partitions(rsh.pure.toSet)
    entailmentLog.printProgress("Unfoldings iterator: Will consider " + spatialPartitions.size + " spatial and " + purePartitions.size + " pure partitions of " + rsh)
//    entailmentLog.printProgress(spatialPartitions.mkString("Spatial: ", "\n", ""))
//    entailmentLog.printProgress(purePartitions.mkString("Pure: ", "\n", ""))

    for {
      (sigma1, sigma2) <- spatialPartitions
      (pi1, pi2) <- purePartitions

      if config.keepPartition(sigma1, pi1)
      representative = SymbolicHeap(pi1.toSeq, sigma1.toSeq, Seq.empty)
      extension = SymbolicHeap(pi2.toSeq, sigma2.toSeq, Seq.empty)
      partition <- SymbolicHeapPartition.partitionsFromUnbindingSharedVars(representative, extension)
      if partition.repFV <= maxNumFv
    } yield partition
  }

}
