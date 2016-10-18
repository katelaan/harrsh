package slex.heapautomata

import slex.Combinators
import slex.heapautomata.utils.{EqualityUtils, UnsafeAtomsAsClosure}
import slex.main.SlexLogging
import slex.seplog.{SymbolicHeap, _}

/**
  * Created by jkatelaa on 10/18/16.
  */
class BaseTrackingAutomaton(
                             numFV : Int,
                             isFinalPredicate : (BaseTrackingAutomaton, Set[FV], Set[PureAtom]) => Boolean,
                             override val description : String = "TRACK-BASE"
                           ) extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import BaseTrackingAutomaton._

  override type State = TrackingInfo

  lazy val InconsistentState : State = inconsistentTrackingInfo(numFV)

  override lazy val states: Set[State] = computeTrackingStateSpace(numFV)

  override def isFinal(s: State): Boolean = isFinalPredicate(this, s._1, s._2)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.calledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.calledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val consistencyCheckedState = compressAndPropagate(src, lab, InconsistentState)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = dropNonFreeVariables(consistencyCheckedState)

    if (logger.underlying.isDebugEnabled && consistencyCheckedState != trg)
      logger.debug("After dropping bound variables: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set(trg)
  }


}

object BaseTrackingAutomaton extends SlexLogging {

  type TrackingInfo = (Set[FV], Set[PureAtom])

  def inconsistentTrackingInfo(numFV : Int) : TrackingInfo = (Set(), Set() ++ allFVs(numFV) map (fv => PtrNEq(fv,fv)))

  def allFVs(numFV : Int) = (0 to numFV) map fv

  def computeTrackingStateSpace(numFV : Int) =
    for {
      // TODO: This also computes plenty (but not all) inconsistent states
      alloc <- Combinators.powerSet(Set() ++ ((1 to numFV) map fv))
      pure <- Combinators.powerSet(allEqualitiesOverFVs(numFV))
    } yield (alloc, pure)


  def compressAndPropagate(src : Seq[TrackingInfo], lab : SymbolicHeap, inconsistentState : TrackingInfo) : TrackingInfo = {
    val compressed = compress(lab, src)
    logger.debug("Compressed " + lab + " into " + compressed)

    // Compute allocation set and equalities for compressed SH and compare to target
    val allocExplicit: Seq[FV] = compressed.pointers map (_.from)

    // FIXME: Can we already assume that constraints returned by compression are ordered and thus drop this step?
    val pureExplicit : Set[PureAtom] =  Set() ++ compressed.ptrEqs map orderedAtom

    // Add inequalities for allocated variables
    val inequalitiesFromAlloc : Seq[PureAtom] = Combinators.square(allocExplicit) map {
      case (l,r) => orderedAtom(l, r, false)
    }
    val pureWithAlloc : Set[PureAtom] = pureExplicit ++ inequalitiesFromAlloc

    // Compute fixed point of inequalities and fill up alloc info accordingly
    val stateWithClosure : TrackingInfo = EqualityUtils.propagateConstraints(allocExplicit.toSet, pureWithAlloc)
    logger.debug("State for compressed SH: " + stateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise return state as is
    checkConsistency(stateWithClosure, inconsistentState)
  }

  def dropNonFreeVariables(s : TrackingInfo) : TrackingInfo = {
    (s._1.filter(isFV),
      s._2.filter({
        atom =>
          val (l, r, _) = unwrapAtom(atom)
          isFV(l) && isFV(r)
      }))
  }

  def compress(sh : SymbolicHeap, qs : Seq[TrackingInfo]) : SymbolicHeap = {
    val shFiltered = sh.withoutCalls
    val newHeaps = qs map kernel
    val stateHeapPairs = sh.getCalls zip newHeaps
    val renamedHeaps : Seq[SymbolicHeap] = stateHeapPairs map {
      case (call, heap) =>
        // Rename the free variables of SH to the actual arguments of the predicate calls,
        // i.e. replace the i-th FV with the call argument at index i-1
        val pairs : Seq[(String,String)] = ((1 to call.args.length) map (x => fv(x).toString)) zip (call.args map (_.toString))
        val map : Map[String,String] = Map() ++ pairs
        heap.renameVars(MapBasedRenaming(map))
    }
    val combined = SymbolicHeap.combineAllHeaps(shFiltered +: renamedHeaps)
    combined
  }

  def kernel(s : TrackingInfo) : SymbolicHeap = {
    // FIXME: Here we now assume that the state already contains a closure. If this is not the case, the following does not work.
    //val closure = new ClosureOfAtomSet(pure)
    val closure = UnsafeAtomsAsClosure(s._2)

    val nonredundantAlloc = s._1 filter closure.isMinimumInItsClass

    val alloc : Set[SpatialAtom] = nonredundantAlloc map (p => ptr(p, nil))

    val res = SymbolicHeap(s._2.toSeq, alloc.toSeq)
    logger.debug("Converting " + s + " to " + res)
    res
  }

  def checkConsistency(s : TrackingInfo, inconsistentState : TrackingInfo) : TrackingInfo = {
    if (s._2.exists{
      // Find inequality with two identical arguments
      case PtrNEq(l, r) if l == r => true
      case _ => false
    }) {
      // Inconsistent, return unique inconsistent state
      inconsistentState
    } else {
      // Consistent, return as is
      s
    }
  }

}
