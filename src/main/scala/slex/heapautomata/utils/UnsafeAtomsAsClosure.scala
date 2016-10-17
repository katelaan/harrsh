package slex.heapautomata.utils

import slex.heapautomata._
import slex.seplog.PureAtom

/**
  * Created by jkatelaa on 10/17/16.
  */
case class UnsafeAtomsAsClosure(closure : Set[PureAtom]) extends Closure {

  val CheckSafety = true

  if (CheckSafety) {
    val computedClosure = new ClosureOfAtomSet(closure).asSetOfAtoms
    if (closure != computedClosure)
      throw new IllegalStateException("Assumed " + closure + " is closure, but actual closure is" + computedClosure)
  }

  override def getEqualityClass(fv: FV): Set[FV] = {
    val otherMembers = closure.filter({
      case atom =>
        val (l,r,isEq) = unwrapAtom(atom)
        // Find those equalities that mention fv
        isEq && (l == fv || r == fv)
    }).map({
      case atom =>
        val (l,r,_) = unwrapAtom(atom)
        // Return the argument that is different from fv
        if (l == fv) r else l
    })
    Set(fv) union otherMembers
  }

  override def isMinimumInItsClass(fv: FV): Boolean = closure.find({
    case atom =>
      // Search for a smaller equal element
      val (l,r,isEq) = unwrapAtom(atom)
      isEq && r == fv && l < r
  }).isEmpty

  override def asSetOfAtoms: Set[PureAtom] = closure
}
