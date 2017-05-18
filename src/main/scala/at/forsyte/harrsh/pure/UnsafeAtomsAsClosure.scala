package at.forsyte.harrsh.pure

import at.forsyte.harrsh.main.Config
import at.forsyte.harrsh.pure.EqualityUtils._
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PureAtom}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * Created by jkatelaa on 10/17/16.
  */
private[pure] case class UnsafeAtomsAsClosure(closure : Set[PureAtom]) extends Closure {

  if (Config.HeapAutomataSafeModeEnabled) {
    val computedClosure = Closure.ofSetOfAtoms(closure).asSetOfAtoms
    if (closure != computedClosure)
      throw new IllegalStateException("Assumed " + closure + " is closure, but actual closure is" + computedClosure)
  }

  override def getEquivalenceClass(fv: Var): Set[Var] = {
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

  override def classRepresentativesOf(vars : Set[Var]): Set[Var] = throw new NotImplementedError("To access class representatives, use safe closure implementaton instead")

  override def asSetOfAtoms: Set[PureAtom] = closure

  /**
    * Returns true iff the underlying set of constraints is consistent, i.e., no inequality of the form x != x is implied
    *
    * @return True iff constraints are consistent
    */
  override def isConsistent: Boolean = {
    // TODO Code duplication with ClosureOfAtomSet
    !(asSetOfAtoms.exists(atom => atom.isInstanceOf[PtrEq] && atom.getVarsWithNull.size == 1))
  }
}
