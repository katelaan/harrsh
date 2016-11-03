package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.main.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jkatelaa on 10/17/16.
  */
case class UnsafeAtomsAsClosure(closure : Set[PureAtom]) extends Closure {

  if (HeapAutomataSafeModeEnabled) {
    val computedClosure = new ClosureOfAtomSet(closure).asSetOfAtoms
    if (closure != computedClosure)
      throw new IllegalStateException("Assumed " + closure + " is closure, but actual closure is" + computedClosure)
  }

  override def getEqualityClass(fv: Var): Set[Var] = {
    val otherMembers = closure.filter({
      atom =>
        val (l, r, isEq) = unwrapAtom(atom)
        // Find those equalities that mention fv
        isEq && (l == fv || r == fv)
    }).map({
      atom =>
        val (l, r, _) = unwrapAtom(atom)
        // Return the argument that is different from fv
        if (l == fv) r else l
    })
    Set(fv) union otherMembers
  }

  override def isMinimumInItsClass(fv: Var): Boolean = !closure.exists({
    atom =>
      // Search for a smaller equal element
      val (l, r, isEq) = unwrapAtom(atom)
      isEq && r == fv && l < r
  })

  override def asSetOfAtoms: Set[PureAtom] = closure
}
