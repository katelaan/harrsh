package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{Closure, ConstraintPropagation, EqualityUtils}
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 3/28/17.
  */
case class TrackingInfo private (alloc: Set[Var], pure: Set[PureAtom]) extends Kernelizable with HarrshLogging {

  def equalities : Set[PureAtom] = pure.filter(_.isEquality)

  def dropNonFreeVariables : TrackingInfo = {
    TrackingInfo(alloc.filter(_.isFree),
      pure.filter{
        atom =>
          val (l, r, _) = EqualityUtils.unwrapAtom(atom)
          l.isFree && r.isFree
      })
  }

  lazy val isConsistent : Boolean =
    !pure.exists {
      // Find inequality with two identical arguments
      case PureAtom(l, r, false) if l == r => true
      case _ => false
    } && !alloc.contains(Var.nil)

  def projectionToFreeVars : TrackingInfo = TrackingInfo(alloc.filter(_.isFree), pure.filter(_.comparesFree))

  override def kernel : SymbolicHeap = {
    // Here we assume that the state already contains a closure. If this is not the case, the following does not work.
    val closure = Closure.unsafeTrivialClosure(pure)

    val nonredundantAlloc = alloc filter closure.isRepresentative

    val allocPtr : Set[PointsTo] = nonredundantAlloc map (p => PointsTo(p, nil))

    val res = SymbolicHeap(pure.toSeq, allocPtr.toSeq, Seq.empty)
    logger.debug("Converting " + this + " to " + res)
    res
  }

}

object TrackingInfo {

  def fromSymbolicHeap(sh : SymbolicHeap) : TrackingInfo = {
    // Compute allocation set and equalities for compressed SH and compare to target
    val allocExplicit: Seq[Var] = sh.pointers map (_.fromAsVar)

    // TODO: Ensure that we can already assume that constraints returned by compression are ordered and thus drop this step
    val pureExplicit : Set[PureAtom] =  Set() ++ sh.ptrComparisons map EqualityUtils.orderedAtom

    // Add inequalities for allocated variables
    val inequalitiesFromAlloc : Seq[PureAtom] = Combinators.square(allocExplicit) map {
      case (l,r) => EqualityUtils.orderedAtom(l, r, isEqual = false)
    }
    val pureWithAlloc : Set[PureAtom] = pureExplicit ++ inequalitiesFromAlloc

    // Compute fixed point of inequalities and fill up alloc info accordingly
    val (alloc, pure) = ConstraintPropagation.propagateConstraints(allocExplicit.toSet, pureWithAlloc)
    TrackingInfo(alloc, pure)
  }

  def fromPair = TrackingInfo

  def inconsistentTrackingInfo(numFV : Int) : TrackingInfo = TrackingInfo(Set(), Set() ++ mkAllVars(0 to numFV) map (fv => PtrNEq(fv,fv)))

}