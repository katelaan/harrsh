package slex.heapautomata

import slex.seplog._
import slex.seplog.inductive._
import slex.main.SlexLogging
import BaseTrackingAutomaton._
import slex.heapautomata.utils.{EqualityUtils, ReachabilityMatrix, UnsafeAtomsAsClosure}
import slex.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/19/16.
  */
class BaseReachabilityAutomaton[A](
                                 numFV : Int,
                                 isFinalPredicate : (BaseReachabilityAutomaton[A], BaseReachabilityAutomaton.ReachabilityInfo, A) => Boolean,
                                 tagComputation : (Seq[A], TrackingInfo, Set[(FV,FV)], Set[FV]) => A,
                                 inconsistentTag : A,
                                 valsOfTag : Set[A],
                                 override val description : String = "BASE-REACH") extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import BaseReachabilityAutomaton._

  override type State = (ReachabilityInfo, A)

  lazy val InconsistentState : State = ((inconsistentTrackingInfo(numFV),inconsistentReachability(numFV)), inconsistentTag)

  override lazy val states: Set[State] = for {
    track <- computeTrackingStateSpace(numFV)
    reach <- ReachabilityMatrix.allMatrices(numFV)
    tag <- valsOfTag
  } yield ((track, reach), tag)

  override def isFinal(s: State): Boolean = isFinalPredicate(this, s._1, s._2)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.calledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.calledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val (consistencyCheckedState,tag) = compressAndPropagateReachability(src, lab, InconsistentState, numFV, tagComputation)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = (dropNonFreeVariables(consistencyCheckedState._1), consistencyCheckedState._2)

    logger.debug("Target state: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set((trg,tag))
  }
}

object BaseReachabilityAutomaton extends SlexLogging {

  type ReachabilityInfo = (TrackingInfo,ReachabilityMatrix)

  // In an inconsistent state, everything is reachable
  def inconsistentReachability(numFV : Int) = ReachabilityMatrix(numFV, Array.fill((numFV+1)*(numFV+1))(true))

  def compressAndPropagateReachability[A](src : Seq[(ReachabilityInfo,A)],
                                          lab : SymbolicHeap,
                                          inconsistentState : (ReachabilityInfo,A),
                                          numFV : Int,
                                          tagComputation : (Seq[A], TrackingInfo, Set[(FV,FV)], Set[FV]) => A) : (ReachabilityInfo,A) = {
    val compressed = reachabilityCompression(lab, src map (_._1))
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
    val trackingsStateWithClosure : TrackingInfo = EqualityUtils.propagateConstraints(allocExplicit.toSet, pureWithAlloc)
    logger.debug("Tracking info for compressed SH: " + trackingsStateWithClosure)

    // TODO Reduce code duplication wrt BaseTracking. The following part is the only one that is new to reachability
    // If the state is inconsistent, return the unique inconsistent state; otherwise compute reachability info
    if (isConsistent(trackingsStateWithClosure)) {
      // Compute reachability info by following pointers
      val (pairs, newMatrix) = reachabilityFixedPoint(numFV, compressed, trackingsStateWithClosure)
      //tagComputation : (Seq[A], TrackingInfo, Set[(FV,FV)], Set[FV]) => A,
      val tag = tagComputation(src map (_._2), trackingsStateWithClosure, pairs, compressed.getVars map PtrVar)
      ((trackingsStateWithClosure, newMatrix), tag)
    } else inconsistentState
  }

  def reachabilityFixedPoint(numFV : Int, compressedHeap : SymbolicHeap, tracking : TrackingInfo) : (Set[(FV,FV)], ReachabilityMatrix) = {

    def ptrToPairs(ptr : PointsTo) : Seq[(FV,FV)] = ptr.to map ((ptr.from, _))

    val directReachability : Seq[(FV,FV)] = compressedHeap.pointers flatMap ptrToPairs
    val equalities = tracking._2.filter(_.isInstanceOf[PtrEq]).map(_.asInstanceOf[PtrEq])
    val pairs = reachabilityFixedPoint(compressedHeap, equalities, directReachability.toSet)
    logger.trace("Reached fixed point " + pairs)

    val reach = ReachabilityMatrix.emptyMatrix(numFV)
    for {
      (from, to) <- pairs
      if isFV(from) && isFV(to)
    } {
      reach.update(from, to, setReachable = true)
    }
    logger.trace("Reachability matrix for compressed SH: " + reach)

    (pairs, reach)
  }

  @tailrec
  private def reachabilityFixedPoint(compressedHeap : SymbolicHeap, equalities: Set[PtrEq], pairs : Set[(FV, FV)]) : Set[(FV, FV)] = {

    logger.trace("Iterating reachability computation from " + pairs + " modulo equalities " + equalities)

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
    logger.trace("Equality propagation: " + transitiveEqualityStep)

    // Propagate reachability
    val transitivePointerStep = for {
      (from, to) <- pairs
      (from2, to2) <- pairs
      if to == from2
    } yield (from, to2)
    logger.trace("Pointer propagation: " + transitivePointerStep)

    val newPairs = pairs union transitiveEqualityStep union transitivePointerStep

    if (newPairs == pairs) pairs else reachabilityFixedPoint(compressedHeap, equalities, newPairs)
  }

  def reachabilityCompression(sh : SymbolicHeap, qs : Seq[ReachabilityInfo]) : SymbolicHeap = compressWithKernelization(reachabilityKernel)(sh, qs)

  // TODO Reduce code duplication in kernelization? cf BaseTracking
  // TODO This is the kernel from the paper, i.e. introducing free vars; this is NOT necessary in our implementation with variable-length pointers
  def reachabilityKernel(s : (TrackingInfo, ReachabilityMatrix)) : SymbolicHeap = {
    val ((alloc,pure),reach) = s

    // FIXME: Here we now assume that the state already contains a closure. If this is not the case, the following does not work.
    //val closure = new ClosureOfAtomSet(pure)
    val closure = UnsafeAtomsAsClosure(pure)

    val nonredundantAlloc = alloc filter closure.isMinimumInItsClass

    val freshVar = PtrVar("z")

    val kernelPtrs : Set[SpatialAtom] = nonredundantAlloc map (reachInfoToPtr(_, reach, freshVar))

    val res = SymbolicHeap(pure.toSeq, kernelPtrs.toSeq, Seq(freshVar.id))
    logger.trace("Converting source state " + s + " to " + res)
    res
  }

  private def reachInfoToPtr(src : FV, reach : ReachabilityMatrix, placeholder : FV) : PointsTo = {
    val info : Seq[Boolean] = reach.getRowFor(src)

    val targets = info.zipWithIndex map {
      case (r,i) => if (r) fv(i) else placeholder
    }
    PointsTo(src, targets)
  }

  /**
    * Computes reachability matrix for the given set of variables (possibly including the nullptr)
    * @param ti Tracking information AFTER congruence closure computation
    * @param reachPairs Reachability between pairs of variables AFTER transitive closure computation
    * @param vars Variables to take into account; add nullptr explicitly to have it included
    * @return (variable-to-matrix-index map, matrix)
    */
  def computeExtendedMatrix(ti : TrackingInfo, reachPairs : Set[(FV,FV)], vars : Set[FV]) : (Map[FV, Int], ReachabilityMatrix) = {
    val ixs : Map[FV, Int] = Map() ++ vars.zipWithIndex

    // TODO Code duplication in matrix computation (plus, we're computing a second matrix on top of the FV-reachability matrix...)
    // Note: Subtract 1, because the null pointer is either explicitly in vars, or to be ignored
    val reach = ReachabilityMatrix.emptyMatrix(vars.size - 1)
    for ((from, to) <- reachPairs) {
      reach.update(ixs(from), ixs(to), setReachable = true)
    }

    logger.debug("Extended matrix for variable numbering " + ixs.toSeq.sortBy(_._2).map(p => p._1 + " -> " + p._2).mkString(", ") + ": " + reach)

    (ixs, reach)
  }

  def isGarbageFree(ti : TrackingInfo, reachPairs : Set[(FV,FV)], vars : Set[FV], numFV : Int): Boolean = {

    // FIXME Null handling?

    logger.debug("Computing garbage freedom for variables " + vars)

    lazy val eqs = ti._2.filter(_.isInstanceOf[PtrEq]).map(_.asInstanceOf[PtrEq])

    def isEqualToFV(v : FV) = eqs.exists {
      case PtrEq(left, right) => left == v && isFV(right) || right == v && isFV(left)
    }

    val (ixs, reach) = computeExtendedMatrix(ti, reachPairs, vars)

    // TODO Needlessly inefficient as well...
    def isReachableFromFV(trg : FV) : Boolean = {
      val results : Set[Boolean] = for {
        fv <- vars
        if isFV(fv)
      } yield reach.isReachable(ixs(fv), ixs(trg))

      results.exists(b => b)
    }

    // TODO Stop as soon as garbage is found
    val reachableFromFV = for (v <- vars) yield isFV(v) || isEqualToFV(v) || isReachableFromFV(v)

    val garbageFree = !reachableFromFV.exists(!_)

    if (!garbageFree) {
      logger.debug("Discovered garbage: " + (vars filter (v => !(isFV(v) || isEqualToFV(v) || isReachableFromFV(v)))))
    }

    garbageFree
  }

  def isAcyclic(ti : TrackingInfo, reachPairs : Set[(FV,FV)], vars : Set[FV], numFV : Int): Boolean = {

    // FIXME Null handling?

    logger.debug("Computing acyclicity for variables " + vars)

    val (ixs, reach) = computeExtendedMatrix(ti, reachPairs, vars)

    // TODO Stop as soon as cycle is found (but iterating over everything here is needlessly expensive, but through the also needless transformation to Seq, we at least get nice logging below...)
    val cycles = for (v <- vars.toSeq) yield reach.isReachable(ixs(v), ixs(v))

    logger.debug("Cycles: " + (vars zip cycles))

    !cycles.exists(b => b)
  }

}
