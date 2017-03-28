package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PtrNEq, PureAtom, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 3/28/17.
  */
case class TrackingInfo private (alloc: Set[Var], pure: Set[PureAtom]) {

  def equalities : Set[PtrEq] = pure.filter(_.isInstanceOf[PtrEq]).map(_.asInstanceOf[PtrEq])

  def dropNonFreeVariables : TrackingInfo = {
    TrackingInfo(alloc.filter(isFV),
      pure.filter({
        atom =>
          val (l, r, _) = unwrapAtom(atom)
          isFV(l) && isFV(r)
      }))
  }

  def isConsistent : Boolean =
    !pure.exists {
      // Find inequality with two identical arguments
      case PtrNEq(l, r) if l == r => true
      case _ => false
    }

}

object TrackingInfo {

  def fromSymbolicHeap(sh : SymbolicHeap) : TrackingInfo = {
    // Compute allocation set and equalities for compressed SH and compare to target
    val allocExplicit: Seq[Var] = sh.pointers map (_.fromAsVar)

    // TODO: Ensure that we can already assume that constraints returned by compression are ordered and thus drop this step
    val pureExplicit : Set[PureAtom] =  Set() ++ sh.ptrComparisons map orderedAtom

    // Add inequalities for allocated variables
    val inequalitiesFromAlloc : Seq[PureAtom] = Combinators.square(allocExplicit) map {
      case (l,r) => orderedAtom(l, r, isEqual = false)
    }
    val pureWithAlloc : Set[PureAtom] = pureExplicit ++ inequalitiesFromAlloc

    // Compute fixed point of inequalities and fill up alloc info accordingly
    val (alloc, pure) = EqualityUtils.propagateConstraints(allocExplicit.toSet, pureWithAlloc)
    TrackingInfo(alloc, pure)
  }

  def fromPair = TrackingInfo

  def inconsistentTrackingInfo(numFV : Int) : TrackingInfo = TrackingInfo(Set(), Set() ++ mkAllFVs(numFV) map (fv => PtrNEq(PtrExpr.fromFV(fv),PtrExpr.fromFV(fv))))

}