package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.{PtrExpr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PointsTo, SymbolicHeap}

/**
  * Created by jkatelaa on 3/28/17.
  */
case class ReachabilityInfo(ti : TrackingInfo, rm : ReachabilityMatrix) extends Kernelizable with HarrshLogging {

  def dropNonFreeVaraibles = copy(ti = ti.dropNonFreeVariables)

  // TODO Reduce code duplication in kernelization? cf BaseTracking
  // TODO This is the kernel from the paper, i.e. introducing free vars; this is NOT necessary in our implementation with variable-length pointers
  override def kernel : SymbolicHeap = {
    val TrackingInfo(alloc,pure) = ti

    // Note: Here we now assume that the state already contains a closure. If this is not the case, the following does not work.
    // TODO Just hide this in tracking info, where we know that we have a closure anyway?
    val closure = Closure.unsafeTrivialClosure(pure)

    val nonredundantAlloc = alloc filter closure.isRepresentative

    val kernelPtrs : Set[PointsTo] = nonredundantAlloc map (reachInfoToPtr(_, rm))

    val res = SymbolicHeap(pure.toSeq, kernelPtrs.toSeq, Seq.empty)
    logger.trace("Converting source state " + this + " to " + res)
    res
  }

  private def reachInfoToPtr(src : Var, reach : ReachabilityMatrix) : PointsTo = {
    val info : Seq[Boolean] = reach.getRowFor(src)

    val targets = info.zipWithIndex filter (_._1) map (p=>Var(p._2))

    PointsTo(PtrVar(src), targets map (PtrExpr(_)))
  }

}

object ReachabilityInfo {

  def inconsistentReachabilityInfo(numFV : Int) = ReachabilityInfo(TrackingInfo.inconsistentTrackingInfo(numFV), ReachabilityMatrix.inconsistentReachabilityMatrix(numFV))

}