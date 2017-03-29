package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.{Kernelizable, ReachabilityInfo, ReachabilityMatrix, TrackingInfo}
import at.forsyte.harrsh.heapautomata.{BoundedFvAutomatonWithTargetComputation, TargetComputationWithExtraInformation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/29/17.
  */
class BaseReachabilityAutomaton(numFV : Int) extends BoundedFvAutomatonWithTargetComputation(numFV) with TargetComputationWithExtraInformation[BaseReachabilityAutomaton.ExtraInfo] {

  import BaseReachabilityAutomaton.ExtraInfo

  override val description : String = "BASE-REACH"

  override type State = ReachabilityInfo

  override lazy val InconsistentState : State = ReachabilityInfo.inconsistentReachabilityInfo(numFV)

  override lazy val states: Set[State] = for {
    track <- BaseTrackingAutomaton.computeTrackingStateSpace(numFV)
    reach <- ReachabilityMatrix.allMatrices(numFV)
  } yield ReachabilityInfo(track, reach)

  override def isFinal(s: State): Boolean = throw new IllegalStateException("Base reachability automaton used without providing final states")

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = getTargetsWithExtraInfo(src, lab) map (_._1)

  override def getTargetsWithExtraInfo(src : Seq[State], lab : SymbolicHeap) : Set[(State,ExtraInfo)] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.identsOfCalledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.identsOfCalledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val (consistencyCheckedState,extraInfo) = compressAndPropagateReachability(src, lab, InconsistentState, numFV)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.dropNonFreeVaraibles

    logger.debug("Target state: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set((trg,extraInfo))
  }

  private def compressAndPropagateReachability[A](src : Seq[ReachabilityInfo],
                                                  lab : SymbolicHeap,
                                                  inconsistentState : ReachabilityInfo,
                                                  numFV : Int) : (ReachabilityInfo,ExtraInfo) =
  {
    val compressed = Kernelizable.compressByKernelization(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)

    // Compute fixed point of inequalities and fill up alloc info accordingly
    val trackingsStateWithClosure : TrackingInfo = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("Tracking info for compressed SH: " + trackingsStateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise compute reachability info
    if (trackingsStateWithClosure.isConsistent) {
      // Compute reachability info by following pointers
      val newMatrix = ReachabilityMatrix.fromSymbolicHeapAndTrackingInfo(numFV, compressed, trackingsStateWithClosure)
      val extraInfo = ExtraInfo(trackingsStateWithClosure, newMatrix.underlyingPairs.get, compressed.allVars)
      (ReachabilityInfo(trackingsStateWithClosure, newMatrix), extraInfo)
    } else (inconsistentState,ExtraInfo(trackingsStateWithClosure, Set.empty, compressed.allVars))
  }

}

object BaseReachabilityAutomaton extends HarrshLogging {

  // TODO Get rid of the reachability pairs either here or in the matrix class?
  case class ExtraInfo(fullTrackingInfoWithBoundVars : TrackingInfo, reachabilityPairs : Set[(Var,Var)], allVars : Set[Var])


}