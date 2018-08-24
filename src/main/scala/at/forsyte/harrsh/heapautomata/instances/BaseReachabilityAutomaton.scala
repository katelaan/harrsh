package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.{Kernelizable, ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState, TaggedTargetComputation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/29/17.
  */
class BaseReachabilityAutomaton() extends HeapAutomaton with InconsistentState with TaggedTargetComputation[BaseReachabilityAutomaton.UncleanedTrackingInfo] {

  import BaseReachabilityAutomaton.UncleanedTrackingInfo

  override val description : String = "BASE-REACH"

  override type State = ReachabilityInfo

  override def inconsistentState(fvs: Seq[FreeVar]): State = ReachabilityInfo.inconsistentReachabilityInfo(fvs)

  override def isFinal(s: State): Boolean = throw new IllegalStateException("Base reachability automaton used without providing final states")

  override def getTargetsWithTags(src : Seq[State], lab : SymbolicHeap) : Set[(State,UncleanedTrackingInfo)] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.identsOfCalledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.identsOfCalledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val (consistencyCheckedState,extraInfo) = compressAndPropagateReachability(src, lab)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.projectionToFreeVars

    logger.debug("Target state: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set((trg,extraInfo))
  }

  private def compressAndPropagateReachability[A](src : Seq[ReachabilityInfo], lab : SymbolicHeap) : (ReachabilityInfo,UncleanedTrackingInfo) =
  {
    val compressed = Kernelizable.compressByKernelization(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)
    val trackingsStateWithClosure : TrackingInfo = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("Tracking info for compressed SH: " + trackingsStateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise compute reachability info
    if (trackingsStateWithClosure.isConsistent) {
      val newMatrix = ReachabilityMatrix.fromSymbolicHeapAndTrackingInfo(compressed, trackingsStateWithClosure)
      val extraInfo = UncleanedTrackingInfo(trackingsStateWithClosure, compressed.allNonNullVars)
      (ReachabilityInfo(trackingsStateWithClosure, newMatrix), extraInfo)
    } else {
      (inconsistentState(lab.freeVars),UncleanedTrackingInfo(trackingsStateWithClosure, compressed.allNonNullVars))
    }
  }

}

object BaseReachabilityAutomaton extends HarrshLogging {

  case class UncleanedTrackingInfo(fullTrackingInfoWithBoundVars : TrackingInfo, allVars : Set[Var])

}