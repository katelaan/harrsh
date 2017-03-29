package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.{Kernelizable, TrackingInfo}
import at.forsyte.harrsh.heapautomata.{BoundedFvAutomatonWithTargetComputation, TargetComputationWithExtraInformation}
import at.forsyte.harrsh.heapautomata.instances.BaseReachabilityAutomaton.ExtraInfo
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityUtils
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 10/18/16.
  */
abstract class BaseTrackingAutomaton(numFV : Int) extends BoundedFvAutomatonWithTargetComputation(numFV) with TargetComputationWithExtraInformation[TrackingInfo] {

  override type State = TrackingInfo

  override val description : String = "TRACK-BASE"

  lazy val InconsistentState : State = TrackingInfo.inconsistentTrackingInfo(numFV)

  override lazy val states: Set[State] = BaseTrackingAutomaton.computeTrackingStateSpace(numFV)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = getTargetsWithExtraInfo(src, lab) map (_._1)

  override def getTargetsWithExtraInfo(src : Seq[State], lab : SymbolicHeap) : Set[(State,TrackingInfo)] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.identsOfCalledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.identsOfCalledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val consistencyCheckedState = BaseTrackingAutomaton.compressAndPropagateTracking(src, lab, InconsistentState)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.dropNonFreeVariables

    if (logger.underlying.isDebugEnabled && consistencyCheckedState != trg)
      logger.debug("After dropping bound variables: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set((trg,consistencyCheckedState))
  }

}

object BaseTrackingAutomaton extends HarrshLogging {

  def computeTrackingStateSpace(numFV : Int) =
    for {
      // TODO: This also computes plenty (but not all) inconsistent states
      alloc <- Combinators.powerSet(Set() ++ ((1 to numFV) map Var.mkVar))
      pure <- Combinators.powerSet(EqualityUtils.allEqualitiesOverFVs(numFV))
    } yield TrackingInfo.fromPair(alloc, pure)

  def compressAndPropagateTracking(src : Seq[TrackingInfo], lab : SymbolicHeap, inconsistentState : TrackingInfo) : TrackingInfo = {
    val compressed = Kernelizable.compressByKernelization(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)

    val stateWithClosure = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("State for compressed SH: " + stateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise return state as is
    if (stateWithClosure.isConsistent) stateWithClosure else inconsistentState
  }

  def defaultTrackingAutomaton(numFV : Int) : BaseTrackingAutomaton = new BaseTrackingAutomaton(numFV) {
    override def isFinal(s: TrackingInfo): Boolean = throw new IllegalStateException("Call to default implementation -- should have been overridden")
  }

}
