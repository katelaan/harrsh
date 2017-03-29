package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.TaggedAutomaton
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityMatrix, StateTag, TrackingInfo}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/29/17.
  */
class AcyclicityAutomaton(numFV : Int, negate : Boolean) extends TaggedAutomaton[Boolean, BaseReachabilityAutomaton.ExtraInfo, BaseReachabilityAutomaton](numFV) {
  override val baseAutomaton = new BaseReachabilityAutomaton(numFV)

  override val tags = StateTag.instances.booleanTag

  override val description = (if (negate) "CYC_" else "ACYC_") + numFV

  override def tagComputation(srcTags: Seq[Boolean], lab : SymbolicHeap, baseTrg: baseAutomaton.State, ei : BaseReachabilityAutomaton.ExtraInfo): Boolean = {
    if (negate) {
      srcTags.exists(b => b) || !isAcyclic(ei.fullTrackingInfoWithBoundVars, ei.reachabilityPairs, ei.allVars + Var.mkVar(0), numFV)
    } else {
      !srcTags.exists(!_) && isAcyclic(ei.fullTrackingInfoWithBoundVars, ei.reachabilityPairs, ei.allVars + Var.mkVar(0), numFV)
    }
}

  private def isAcyclic(ti : TrackingInfo, reachPairs : Set[(Var,Var)], vars : Set[Var], numFV : Int): Boolean = {
    if (!ti.isConsistent) {
      // TODO Does this yield the correct results in the negated setting as well?
      true
    } else {
      // FIXME Null handling?

      logger.debug("Computing acyclicity for variables " + vars)

      val (ixs, reach) = ReachabilityMatrix.computeExtendedMatrix(ti, reachPairs, vars)

      // TODO Stop as soon as cycle is found (but iterating over everything here is needlessly expensive, but through the also needless transformation to Seq, we at least get nice logging below...)
      val cycles = for (v <- vars.toSeq) yield reach.isReachable(ixs(v), ixs(v))

      logger.debug("Cycles: " + (vars zip cycles))

      !cycles.exists(b => b)
    }
  }

}
