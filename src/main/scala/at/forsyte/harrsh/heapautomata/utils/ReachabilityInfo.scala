package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PointsTo, SymbolicHeap}

/**
  * Created by jkatelaa on 3/28/17.
  */
case class ReachabilityInfo(ti : TrackingInfo, rm : ReachabilityMatrix) extends Kernelizable with HarrshLogging {

  def projectionToFreeVars = copy(ti = ti.projectionToFreeVars)

  // TODO Reduce code duplication in kernelization? cf BaseTracking
  override def kernel : SymbolicHeap = {
    val TrackingInfo(alloc,pure,freeVars) = ti

    // Note: Here we now assume that the state already contains a closure. If this is not the case, the following does not work.
    // TODO Just hide this in tracking info, where we know that we have a closure anyway?
    val closure = Closure.unsafeTrivialClosure(pure)

    val nonredundantAlloc = alloc filter closure.isRepresentative

    val kernelPtrs : Set[PointsTo] = nonredundantAlloc map (reachInfoToPtr(_, rm))

    val res = SymbolicHeap(pure.toSeq, kernelPtrs.toSeq, Seq.empty, freeVars)
    logger.trace("Converting source state " + this + " to " + res)
    res
  }

  private def reachInfoToPtr(src : Var, reach : ReachabilityMatrix) : PointsTo = {
    val targets = reach.reachableFrom(src).toSeq
    PointsTo(src, targets)
  }

}

object ReachabilityInfo {

  def inconsistentReachabilityInfo(fvs: Seq[FreeVar]) = ReachabilityInfo(TrackingInfo.inconsistentTrackingInfo(fvs), ReachabilityMatrix.emptyMatrix)

}