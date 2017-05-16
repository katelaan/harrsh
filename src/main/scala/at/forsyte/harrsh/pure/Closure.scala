package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Created by jkatelaa on 10/17/16.
  */
trait Closure {

  /**
    * Converts the closure to an explicit representation as set of atomic constraints
    * @return Set of pure atoms that makes explicit all pairwise (in)equalities implied by the congruence
    */
  def asSetOfAtoms : Set[PureAtom]

  /**
    * Computes the set of all variables that are in the same equivalence class as fv
    * @param fv Member of class
    * @return Equivalence class that contains fv
    */
  def getEquivalenceClass(fv : Var) : Set[Var]

  /**
    * Checks if fv is the (numerically) minimal element in its equivalence class
    * @param fv Member of class
    * @return True iff fv is the (numerically) minimal element in its equivalence class
    */
  def isMinimumInItsClass(fv : Var) : Boolean

  /**
    * Returns true iff the underlying set of constraints is consistent, i.e., no inequality of the form x != x is implied
    * @return True iff constraints are consistent
    */
  def isConsistent : Boolean

}

object Closure {

  /**
    * Computes the congruence closure of atoms
    * @param atoms Atoms whose closure should be computed
    * @return Closure of atoms
    */
  def ofSetOfAtoms(atoms : Set[PureAtom]) : Closure = ClosureOfAtomSet(atoms)

  /**
    * Treats the given set of atoms as closure; warning: If it is not a closure, this will not be detected
    * @param atoms Congruence-closed set of atoms
    * @return View of atoms as closure
    */
  def unsafeTrivialClosure(atoms : Set[PureAtom]) : Closure = UnsafeAtomsAsClosure(atoms)

}