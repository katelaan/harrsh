package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.heapautomata.utils.{EqualityUtils, TrackingInfo, UnsafeAtomsAsClosure}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.{MapBasedRenaming, PtrExpr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 10/18/16.
  */
class BaseTrackingAutomaton(
                             numFV : Int,
                             isFinalPredicate : (BaseTrackingAutomaton, Set[Var], Set[PureAtom]) => Boolean,
                             override val description : String = "TRACK-BASE"
                           ) extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import BaseTrackingAutomaton._

  override type State = TrackingInfo

  lazy val InconsistentState : State = TrackingInfo.inconsistentTrackingInfo(numFV)

  override lazy val states: Set[State] = computeTrackingStateSpace(numFV)

  override def isFinal(s: State): Boolean = isFinalPredicate(this, s.alloc, s.pure)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.identsOfCalledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.identsOfCalledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val consistencyCheckedState = compressAndPropagateTracking(src, lab, InconsistentState)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.dropNonFreeVariables

    if (logger.underlying.isDebugEnabled && consistencyCheckedState != trg)
      logger.debug("After dropping bound variables: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set(trg)
  }


}

object BaseTrackingAutomaton extends HarrshLogging {

  def computeTrackingStateSpace(numFV : Int) =
    for {
      // TODO: This also computes plenty (but not all) inconsistent states
      alloc <- Combinators.powerSet(Set() ++ ((1 to numFV) map mkVar))
      pure <- Combinators.powerSet(allEqualitiesOverFVs(numFV))
    } yield TrackingInfo.fromPair(alloc, pure)


  def compressAndPropagateTracking(src : Seq[TrackingInfo], lab : SymbolicHeap, inconsistentState : TrackingInfo) : TrackingInfo = {
    val compressed = trackingCompression(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)

    val stateWithClosure = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("State for compressed SH: " + stateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise return state as is
    if (stateWithClosure.isConsistent) stateWithClosure else inconsistentState
  }

  def trackingCompression(sh : SymbolicHeap, qs : Seq[TrackingInfo]) : SymbolicHeap = compressWithQuantifierFreeKernel(trackingKernel)(sh, qs)

  def compressWithQuantifierFreeKernel[A](kernelization : A => SymbolicHeap)(sh : SymbolicHeap, qs : Seq[A]) : SymbolicHeap = {
    val newHeaps = qs map kernelization
    // Since we deal only with quantifier-free kernels, no alpha-conversion is necessary
    sh.replaceCalls(newHeaps, performAlphaConversion = false)
  }

  def trackingKernel(s : TrackingInfo) : SymbolicHeap = {
    // Here we assume that the state already contains a closure. If this is not the case, the following does not work.
    //val closure = new ClosureOfAtomSet(pure)
    val closure = UnsafeAtomsAsClosure(s.pure)

    val nonredundantAlloc = s.alloc filter closure.isMinimumInItsClass

    val alloc : Set[PointsTo] = nonredundantAlloc map (p => ptr(p, nil))

    val res = SymbolicHeap(s.pure.toSeq, alloc.toSeq, Seq.empty)
    logger.debug("Converting " + s + " to " + res)
    res
  }



}
