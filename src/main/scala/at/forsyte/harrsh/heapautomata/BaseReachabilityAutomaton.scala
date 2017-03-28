package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.main.HarrshLogging
import Var._
import BaseTrackingAutomaton._
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityInfo, ReachabilityMatrix, TrackingInfo, UnsafeAtomsAsClosure}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/19/16.
  */
class BaseReachabilityAutomaton[A](
                                    numFV : Int,
                                    isFinalPredicate : (BaseReachabilityAutomaton[A], ReachabilityInfo, A) => Boolean,
                                  // TODO Why do we have the set of pairs in the tag computation?
                                    tagComputation : (Seq[A], TrackingInfo, Set[(Var,Var)], Set[Var]) => A,
                                    inconsistentTag : A,
                                    valsOfTag : Set[A],
                                    override val description : String = "BASE-REACH") extends BoundedFvAutomatonWithTargetComputation(numFV) {

  import BaseReachabilityAutomaton._

  override type State = (ReachabilityInfo, A)

  lazy val InconsistentState : State = (ReachabilityInfo.inconsistentReachabilityInfo(numFV), inconsistentTag)

  override lazy val states: Set[State] = for {
    track <- computeTrackingStateSpace(numFV)
    reach <- ReachabilityMatrix.allMatrices(numFV)
    tag <- valsOfTag
  } yield (ReachabilityInfo(track, reach), tag)

  override def isFinal(s: State): Boolean = isFinalPredicate(this, s._1, s._2)

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    logger.debug("Computing possible targets " + src.mkString(", ") + " --[" + lab + "]--> ???")
    if (src.length != lab.identsOfCalledPreds.length) throw new IllegalStateException("Number of predicate calls " + lab.identsOfCalledPreds.length + " does not match arity of source state sequence " + src.length)

    // Perform compression + subsequent equality/allocation propagation
    val (consistencyCheckedState,tag) = compressAndPropagateReachability(src, lab, InconsistentState, numFV, tagComputation)
    // Break state down to only the free variables; the other information is not kept in the state space
    val trg = consistencyCheckedState.dropNonFreeVaraibles

    logger.debug("Target state: " + trg)

    // There is a unique target state because we always compute the congruence closure
    Set((trg,tag))
  }
}

object BaseReachabilityAutomaton extends HarrshLogging {

  def compressAndPropagateReachability[A](src : Seq[(ReachabilityInfo,A)],
                                          lab : SymbolicHeap,
                                          inconsistentState : (ReachabilityInfo,A),
                                          numFV : Int,
                                          tagComputation : (Seq[A], TrackingInfo, Set[(Var,Var)], Set[Var]) => A) : (ReachabilityInfo,A) = {
    val compressed = reachabilityCompression(lab, src map (_._1))
    logger.debug("Compressed " + lab + " into " + compressed)

    // Compute fixed point of inequalities and fill up alloc info accordingly
    val trackingsStateWithClosure : TrackingInfo = TrackingInfo.fromSymbolicHeap(compressed)
    logger.debug("Tracking info for compressed SH: " + trackingsStateWithClosure)

    // If the state is inconsistent, return the unique inconsistent state; otherwise compute reachability info
    if (trackingsStateWithClosure.isConsistent) {
      // Compute reachability info by following pointers
      val newMatrix = ReachabilityMatrix.fromSymbolicHeapAndTrackingInfo(numFV, compressed, trackingsStateWithClosure)
      //tagComputation : (Seq[A], TrackingInfo, Set[(FV,FV)], Set[FV]) => A,
      val tag = tagComputation(src map (_._2), trackingsStateWithClosure, newMatrix.underlyingPairs.get, compressed.allVars)
      (ReachabilityInfo(trackingsStateWithClosure, newMatrix), tag)
    } else inconsistentState
  }

  def reachabilityCompression(sh : SymbolicHeap, qs : Seq[ReachabilityInfo]) : SymbolicHeap = compressWithQuantifierFreeKernel(reachabilityKernel)(sh, qs)

  // TODO Reduce code duplication in kernelization? cf BaseTracking
  // TODO This is the kernel from the paper, i.e. introducing free vars; this is NOT necessary in our implementation with variable-length pointers
  def reachabilityKernel(s : ReachabilityInfo) : SymbolicHeap = {
    val ReachabilityInfo(TrackingInfo(alloc,pure),reach) = s

    // Note: Here we now assume that the state already contains a closure. If this is not the case, the following does not work.
    // TODO Just hide this in tracking info, where we know that we have a closure anyway?
    val closure = UnsafeAtomsAsClosure(pure)

    val nonredundantAlloc = alloc filter closure.isMinimumInItsClass

    val kernelPtrs : Set[PointsTo] = nonredundantAlloc map (reachInfoToPtr(_, reach))

    val res = SymbolicHeap(pure.toSeq, kernelPtrs.toSeq, Seq.empty)
    logger.trace("Converting source state " + s + " to " + res)
    res
  }

  private def reachInfoToPtr(src : Var, reach : ReachabilityMatrix) : PointsTo = {
    val info : Seq[Boolean] = reach.getRowFor(src)

    val targets = info.zipWithIndex filter (_._1) map (p=>mkVar(p._2))

    PointsTo(PtrVar(src), targets map PtrExpr.fromFV)
  }

  /**
    * Computes reachability matrix for the given set of variables (possibly including the nullptr)
    * @param ti Tracking information AFTER congruence closure computation
    * @param reachPairs Reachability between pairs of variables AFTER transitive closure computation
    * @param vars Variables to take into account; add nullptr explicitly to have it included
    * @return (variable-to-matrix-index map, matrix)
    */
  def computeExtendedMatrix(ti : TrackingInfo, reachPairs : Set[(Var,Var)], vars : Set[Var]) : (Map[Var, Int], ReachabilityMatrix) = {
    val ixs : Map[Var, Int] = Map() ++ vars.zipWithIndex

    // TODO Code duplication in matrix computation (plus, we're computing a second matrix on top of the FV-reachability matrix...)
    // Note: Subtract 1, because the null pointer is either explicitly in vars, or to be ignored
    val reach = ReachabilityMatrix.emptyMatrix(vars.size - 1)
    for ((from, to) <- reachPairs) {
      reach.update(ixs(from), ixs(to), setReachable = true)
    }

    logger.debug("Extended matrix for variable numbering " + ixs.toSeq.sortBy(_._2).map(p => p._1 + " -> " + p._2).mkString(", ") + ": " + reach)

    (ixs, reach)
  }

  def isGarbageFree(ti : TrackingInfo, reachPairs : Set[(Var,Var)], vars : Set[Var], numFV : Int): Boolean = {

    // FIXME Null handling?

    logger.debug("Computing garbage freedom for variables " + vars)

    lazy val eqs : Set[(Var,Var)] = ti.equalities.map(atom => (atom.l.getVarOrZero,atom.r.getVarOrZero))

    def isEqualToFV(v : Var) = eqs.exists {
      case (left, right) => left == v && isFV(right) || right == v && isFV(left)
    }

    val (ixs, reach) = computeExtendedMatrix(ti, reachPairs, vars)

    // TODO Needlessly inefficient as well...
    def isReachableFromFV(trg : Var) : Boolean = {
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

  def isAcyclic(ti : TrackingInfo, reachPairs : Set[(Var,Var)], vars : Set[Var], numFV : Int): Boolean = {

    // FIXME Null handling?

    logger.debug("Computing acyclicity for variables " + vars)

    val (ixs, reach) = computeExtendedMatrix(ti, reachPairs, vars)

    // TODO Stop as soon as cycle is found (but iterating over everything here is needlessly expensive, but through the also needless transformation to Seq, we at least get nice logging below...)
    val cycles = for (v <- vars.toSeq) yield reach.isReachable(ixs(v), ixs(v))

    logger.debug("Cycles: " + (vars zip cycles))

    !cycles.exists(b => b)
  }

}
