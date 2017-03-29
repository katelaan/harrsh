package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.{Kernelizable, TrackingInfo}
import at.forsyte.harrsh.heapautomata.{FVBound, HeapAutomaton, InconsistentState, TaggedTargetComputation}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityUtils
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 10/18/16.
  */
abstract class BaseTrackingAutomaton(val numFV : Int) extends HeapAutomaton with FVBound with InconsistentState with TaggedTargetComputation[TrackingInfo] {

  override type State = TrackingInfo

  override val description : String = "TRACK-BASE"

  override lazy val inconsistentState : State = TrackingInfo.inconsistentTrackingInfo(numFV)

  override lazy val states: Set[State] = for {
    // This also computes plenty (but not all) inconsistent states, but we should actually not call this ever anyway
    alloc <- Combinators.powerSet(Set() ++ ((1 to numFV) map Var.mkVar))
    pure <- Combinators.powerSet(EqualityUtils.allEqualitiesOverFVs(numFV))
  } yield TrackingInfo.fromPair(alloc, pure)

  override def getTargetsWithTags(src : Seq[State], lab : SymbolicHeap) : Set[(State,TrackingInfo)] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.identsOfCalledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.identsOfCalledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val consistencyCheckedState = compressAndPropagateTracking(src, lab)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.dropNonFreeVariables

    if (logger.underlying.isDebugEnabled && consistencyCheckedState != trg)
      logger.debug("After dropping bound variables: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set((trg,consistencyCheckedState))
  }

  private def compressAndPropagateTracking(src : Seq[TrackingInfo], lab : SymbolicHeap) : TrackingInfo = {
    val compressed = Kernelizable.compressByKernelization(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)

    val stateWithClosure = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("State for compressed SH: " + stateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise return state as is
    if (stateWithClosure.isConsistent) stateWithClosure else inconsistentState
  }

}

object BaseTrackingAutomaton extends HarrshLogging {

  def defaultTrackingAutomaton(numFV : Int) : BaseTrackingAutomaton = new BaseTrackingAutomaton(numFV) {
    override def isFinal(s: TrackingInfo): Boolean = throw new IllegalStateException("Call to default implementation -- should have been overridden")
  }

}
