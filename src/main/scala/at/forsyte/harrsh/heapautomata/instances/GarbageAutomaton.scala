package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.TaggedAutomaton
import at.forsyte.harrsh.heapautomata.utils.{ReachabilityMatrix, StateTag, TrackingInfo}
import at.forsyte.harrsh.refinement.AutomatonTask
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/29/17.
  */
class GarbageAutomaton(negate : Boolean) extends TaggedAutomaton[Boolean, BaseReachabilityAutomaton.UncleanedTrackingInfo, BaseReachabilityAutomaton] {

  override val baseAutomaton = new BaseReachabilityAutomaton()

  override val tags = StateTag.instances.booleanTag

  override val description = if (negate) AutomatonTask.keywords.garb else AutomatonTask.keywords.gf

  override def tagComputation(srcTags: Seq[Boolean], lab : SymbolicHeap, baseTrg: baseAutomaton.State, ei : BaseReachabilityAutomaton.UncleanedTrackingInfo): Boolean = {
    val res = if (negate) {
      val childGarbage = srcTags.exists(b => b)
      val matrixGarbage = !isGarbageFree(ei.fullTrackingInfoWithBoundVars, baseTrg.rm.underlyingPairs, ei.allVars + NullConst)
      logger.debug("Checking for garbage. In children: " + childGarbage + "; in matrix: " + matrixGarbage)
      childGarbage || matrixGarbage
    } else {
      val childGFree = !srcTags.exists(!_)
      val matrixGFree = isGarbageFree(ei.fullTrackingInfoWithBoundVars, baseTrg.rm.underlyingPairs, ei.allVars + NullConst)
      logger.debug("Checking for garbage freedom. In children: " + childGFree + "; in matrix: " + matrixGFree)
      childGFree && matrixGFree
    }

    res
  }

  private def isGarbageFree(ti : TrackingInfo, reachPairs : Set[(Var,Var)], vars : Set[Var]): Boolean = {

    if (!ti.isConsistent) {
      // TODO Does this yield the correct results in the negated setting as well?
      true
    } else {

      // FIXME Null handling?

      logger.debug("Computing garbage freedom for variables " + vars)

      lazy val eqs: Set[(Var, Var)] = ti.equalities.map(atom => (atom.l, atom.r))

      def isEqualToFV(v: Var) = eqs.exists {
        case (left, right) => left == v && right.isFree || right == v && left.isFree
      }

      val reach = ReachabilityMatrix.computeExtendedMatrix(ti, reachPairs, vars)

      // TODO Needlessly inefficient as well...
      def isReachableFromFV(trg: Var): Boolean = {
        val results: Set[Boolean] = for {
          fv <- vars
          if fv.isFree
        } yield reach.isReachable(fv, trg)

        results.exists(b => b)
      }

      // TODO Stop as soon as garbage is found
      val reachableFromFV = for (v <- vars) yield v.isFree || isEqualToFV(v) || isReachableFromFV(v)

      val garbageFree = !reachableFromFV.exists(!_)

      if (!garbageFree) {
        logger.debug("Discovered garbage: " + (vars filter (v => !v.isFree || isEqualToFV(v) || isReachableFromFV(v))))
      }

      garbageFree
    }
  }

}
