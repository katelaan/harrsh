package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jkatelaa on 10/17/16.
  */
private[pure] case class UnsafeAtomsAsClosure(closure : Set[PureAtom]) extends Closure {

  override def getEquivalenceClass(v: Var, defaultToSingletonClass: Boolean = true): Set[Var] = {
    val otherMembers = closure.filter({
      atom =>
        val PureAtom(l, r, isEq) = atom
        // Find those equalities that mention v
        isEq && (l == v || r == v)
    }).map({
      atom =>
        val PureAtom(l, r, _) = atom
        // Return the argument that is different from v
        if (l == v) r else l
    })
    if (otherMembers.isEmpty && !defaultToSingletonClass) {
      Set.empty
    } else {
      Set(v) union otherMembers
    }
  }

  override def isRepresentative(v: Var): Boolean = !closure.exists({
    atom =>
      // Search for a smaller equal element
      val PureAtom(l, r, isEq) = atom
      isEq && r == v && l < r
  })

  override def classRepresentativesOf(vars : Set[Var]): Set[Var] = throw new NotImplementedError("To access class representatives, use safe closure instead")

  override def asSetOfAtoms: Set[PureAtom] = closure

  /**
    * Returns true iff the underlying set of constraints is consistent, i.e., no inequality of the form x != x is implied
    *
    * @return True iff constraints are consistent
    */
  override def isConsistent: Boolean = {
    // TODO Code duplication with ClosureOfAtomSet
    !asSetOfAtoms.exists(atom => !atom.isEquality && atom.getVars.size == 1)
  }

  /**
    * Returns all equivalence classes that are explicitly stored in this closure. (I.e., no trivial classes)
    *
    * @return Set of all equivalence classes
    */
  override def classes: Set[Set[Var]] = throw new NotImplementedError("To access classes, use safe closure instead")
}
