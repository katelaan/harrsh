package slex.heapautomata

import slex.seplog._
import slex.main.SlexLogging
import BaseTrackingAutomaton._
import slex.Combinators
import slex.heapautomata.utils.{EqualityUtils, ReachabilityMatrix, UnsafeAtomsAsClosure}

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/19/16.
  */
class ReachabilityAutomaton(numFV : Int, from : FV, to : FV) extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import ReachabilityAutomaton._

  override type State = (TrackingInfo, ReachabilityMatrix)

  lazy val InconsistentState : State = (inconsistentTrackingInfo(numFV),inconsistentReachability(numFV))

  override lazy val states: Set[State] = for {
    track <- computeTrackingStateSpace(numFV)
    reach <- ReachabilityMatrix.allMatrices(numFV)
  } yield (track, reach)

  override def isFinal(s: State): Boolean = s._2.isReachable(unFV(from), unFV(to))

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.calledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.calledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val consistencyCheckedState = compressAndPropagateReachability(src, lab, InconsistentState, numFV)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = (dropNonFreeVariables(consistencyCheckedState._1), consistencyCheckedState._2)

    if (logger.underlying.isDebugEnabled && consistencyCheckedState != trg)
      logger.debug("After dropping bound variables: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set(trg)
  }
}

object ReachabilityAutomaton extends SlexLogging {

  type ReachabilityInfo = (TrackingInfo,ReachabilityMatrix)

  // In an inconsistent state, everything is reachable
  def inconsistentReachability(numFV : Int) = ReachabilityMatrix(numFV, Array.fill((numFV+1)*(numFV+1))(true))

  def compressAndPropagateReachability(src : Seq[ReachabilityInfo], lab : SymbolicHeap, inconsistentState : ReachabilityInfo, numFV : Int) : ReachabilityInfo = {
    val compressed = reachabilityCompression(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)

    // Compute allocation set and equalities for compressed SH and compare to target
    val allocExplicit: Seq[FV] = compressed.pointers map (_.from)

    // FIXME: Can we already assume that constraints returned by compression are ordered and thus drop this step?
    val pureExplicit : Set[PureAtom] =  Set() ++ compressed.ptrComparisons map orderedAtom

    // Add inequalities for allocated variables
    val inequalitiesFromAlloc : Seq[PureAtom] = Combinators.square(allocExplicit) map {
      case (l,r) => orderedAtom(l, r, false)
    }
    val pureWithAlloc : Set[PureAtom] = pureExplicit ++ inequalitiesFromAlloc

    // Compute fixed point of inequalities and fill up alloc info accordingly
    val stateWithClosure : TrackingInfo = EqualityUtils.propagateConstraints(allocExplicit.toSet, pureWithAlloc)
    logger.debug("State for compressed SH: " + stateWithClosure)

    // TODO Reduce code duplication wrt BaseTracking. The following part is the only one that is new to reachability
    // If the state is inconsistent, return the unique inconsistent state; otherwise compute reachability info
    if (isConsistent(stateWithClosure)) {
      // Compute reachability info by following pointers
      val newMatrix = reachabilityFixedPoint(numFV, compressed, stateWithClosure)
      (stateWithClosure, newMatrix)
    } else inconsistentState
  }

  def reachabilityFixedPoint(numFV : Int, compressedHeap : SymbolicHeap, tracking : TrackingInfo) : ReachabilityMatrix = {

    def ptrToPairs(ptr : PointsTo) : Seq[(FV,FV)] = ptr.to map ((ptr.from, _))

    val directReachability : Seq[(FV,FV)] = compressedHeap.pointers flatMap ptrToPairs
    val equalities = tracking._2.filter(_.isInstanceOf[PtrEq]).map(_.asInstanceOf[PtrEq])
    val pairs = reachabilityFixedPoint(compressedHeap, equalities, directReachability.toSet)
    logger.debug("Reached fixed point " + pairs)

    val reach = ReachabilityMatrix.emptyMatrix(numFV)
    for {
      (from, to) <- pairs
      if isFV(from) && isFV(to)
    } {
      reach.update(from, to, setReachable = true)
    }
    logger.debug("Converted to matrix " + reach)

    reach
  }

  @tailrec
  private def reachabilityFixedPoint(compressedHeap : SymbolicHeap, equalities: Set[PtrEq], pairs : Set[(FV, FV)]) : Set[(FV, FV)] = {

    logger.debug("Iterating reachability computation from " + pairs + " modulo equalities " + equalities)

    // FIXME: Reachability computation is currently extremely inefficient; should replace with a path search algorithm (that regards equalities as steps as well)
    // Propagate equalities
    val transitiveEqualityStep : Set[(FV,FV)] = (for {
      PtrEq(left, right) <- equalities
      (from, to) <- pairs
      if left == from || left == to || right == from || right == to
    } yield (
      Seq[(FV,FV)]()
        ++ (if (left == from) Seq((right,to)) else Seq())
        ++ (if (right == from) Seq((left,to)) else Seq())
        ++ (if (left == to) Seq((from,right)) else Seq())
        ++ (if (right == to) Seq((from, left)) else Seq()))).flatten
    logger.debug("Equality propagation: " + transitiveEqualityStep)

    // Propagate reachability
    val transitivePointerStep = for {
      (from, to) <- pairs
      (from2, to2) <- pairs
      if to == from2
    } yield (from, to2)
    logger.debug("Pointer propagation: " + transitivePointerStep)

    val newPairs = pairs union transitiveEqualityStep union transitivePointerStep

    if (newPairs == pairs) pairs else reachabilityFixedPoint(compressedHeap, equalities, newPairs)
  }

  def reachabilityCompression(sh : SymbolicHeap, qs : Seq[ReachabilityInfo]) : SymbolicHeap = compressWithKernelization(reachabilityKernel)(sh, qs)

  // TODO: Reduce code duplication in kernelization? cf BaseTracking
  def reachabilityKernel(s : (TrackingInfo, ReachabilityMatrix)) : SymbolicHeap = {
    val ((alloc,pure),reach) = s

    // FIXME: Here we now assume that the state already contains a closure. If this is not the case, the following does not work.
    //val closure = new ClosureOfAtomSet(pure)
    val closure = UnsafeAtomsAsClosure(pure)

    val nonredundantAlloc = alloc filter closure.isMinimumInItsClass

    val freshVar = PtrVar("z")

    val kernelPtrs : Set[SpatialAtom] = nonredundantAlloc map (reachInfoToPtr(_, reach, freshVar))

    val res = SymbolicHeap(pure.toSeq, kernelPtrs.toSeq)
    logger.debug("Converting " + s + " to " + res)
    res
  }

  private def reachInfoToPtr(src : FV, reach : ReachabilityMatrix, placeholder : FV) : PointsTo = {
    val info : Seq[Boolean] = reach.getRowFor(src)

    val targets = info.zipWithIndex map {
      case (r,i) => if (r) fv(i) else placeholder
    }
    PointsTo(src, targets)
  }

}
