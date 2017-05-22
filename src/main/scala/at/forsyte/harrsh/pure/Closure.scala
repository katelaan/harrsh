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
    * Computes the set of all variables that are in the same equivalence class as v
    * @param v Member of class
    * @return Equivalence class that contains v
    */
  def getEquivalenceClass(v : Var) : Set[Var]

  /**
    * Checks if v is the representative ((numerically) minimal element) of its equivalence class
    * @param v Member of class
    * @return True iff v is the (numerically) minimal element in its equivalence class
    */
  def isRepresentative(v : Var) : Boolean

  /**
    * Get representative of the class of v
    * @param v Member of class
    * @return Representative of the class of v
    */
  def getRepresentative(v : Var) : Var = Var.minOf(getEquivalenceClass(v))

  /**
    * Returns one representative per equivalence class (the minimal element).
    * Note that since the full set of defined variables of a heap is not stored in the closure,
    * a parameterless variant of this method would not make sense.
    * @param vars Vars whose representatives will be returned
    * @return Set of representatives
    */
  def classRepresentativesOf(vars : Set[Var]) : Set[Var]

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