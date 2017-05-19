package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PtrNEq, PureAtom}

/**
  * Created by jkatelaa on 10/17/16.
  */
private[pure] case class ClosureOfAtomSet(pure : Set[PureAtom]) extends Closure {

  // TODO: This closure class is quite inefficient, having one copy of each equivalence class per member
  var mapToClasses : Map[Var,Set[Var]] = Map()

  for {
    atom <- pure
    (left, right, isEqual) = EqualityUtils.unwrapAtom(atom)
    if isEqual
  } {
    extendEntry(left, right)
  }

  override def getEquivalenceClass(fv : Var) : Set[Var] = mapToClasses.getOrElse(fv, Set(fv))

  override def isMinimumInItsClass(fv : Var) : Boolean = {
    // If the EQ class is defined, check if i is the representation = the minimum of that class
    // Otherwise, no equality for i has been set, so i is the unique and hence minimal element, so it is the representation
    if (mapToClasses.isDefinedAt(fv)) {
      Var.minOf(mapToClasses(fv)) == fv
    } else {
      true
    }
  }

  override def classRepresentativesOf(vars : Set[Var]) : Set[Var] = {
    // Note: The singleton classes are not represented in the map, so the following code would fail
    // val classes = mapToClasses.values.toSet
    // classes.map(_.min)

    vars filter isMinimumInItsClass
  }

  override def isConsistent : Boolean = {
    // TODO Code duplication with ClosureOfAtomSet
    !asSetOfAtoms.exists(atom => atom.isInstanceOf[PtrNEq] && atom.getVarsWithNull.size == 1)
  }

  private def extendEntry(key : Var, newVal : Var) = {
    val eqClass = if (mapToClasses.isDefinedAt(key)) {
      // Class is already defined, just add the new value
      mapToClasses(key) + newVal
    } else {
      // Key not in any known eq class yet
      // Either have to extend class for val, if known already, or create new class
      if (mapToClasses.isDefinedAt(newVal)) mapToClasses(newVal) + key else Set(key, newVal)
    }

    // Extend entry for all members of the eq class
    for {
      classMember <- eqClass
    } {
      mapToClasses = mapToClasses + (classMember -> eqClass)
    }
  }

  override lazy val asSetOfAtoms: Set[PureAtom] = ConstraintPropagation.propagateConstraints(pure).map(_.ordered)
}

