package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PureAtom}

/**
  * Created by jkatelaa on 10/17/16.
  */
private[pure] case class ClosureOfAtomSet(pure : Set[PureAtom]) extends Closure {

  // TODO: This closure class is quite inefficient, having one copy of each equivalence class per member
  private var mapToClasses : Map[Var,Set[Var]] = Map()

  for {
    PureAtom(left, right, isEqual) <- pure
    if isEqual
  } {
    extendEntry(left, right)
  }

  override def getEquivalenceClass(v : Var, defaultToSingletonClass: Boolean = true) : Set[Var] = {
    mapToClasses.getOrElse(v, if (defaultToSingletonClass) Set(v) else Set.empty)
  }

  override def isRepresentative(v : Var) : Boolean = {
    // If the EQ class is defined, check if i is the representation = the minimum of that class
    // Otherwise, no equality for i has been set, so i is the unique and hence minimal element, so it is the representation
    if (mapToClasses.isDefinedAt(v)) {
      Var.minOf(mapToClasses(v)) == v
    } else {
      true
    }
  }

  override def classRepresentativesOf(vars : Set[Var]) : Set[Var] = {
    // Note: The singleton classes are not represented in the map, so the following code would fail
    // val classes = mapToClasses.values.toSet
    // classes.map(_.min)

    vars filter isRepresentative
  }

  override def isConsistent : Boolean = {
    // TODO Code duplication with UnsafeClosure
    !asSetOfAtoms.exists(atom => !atom.isEquality && atom.getVars.size == 1)
  }

  private def extendEntry(key : Var, newVal : Var) = {
    val eqClass = Set(key,newVal) ++ mapToClasses.getOrElse(key, Set.empty) ++ mapToClasses.getOrElse(newVal, Set.empty)

    // Extend entry for all members of the eq class
    for {
      classMember <- eqClass
    } {
      mapToClasses = mapToClasses + (classMember -> eqClass)
    }
  }

  override lazy val asSetOfAtoms: Set[PureAtom] = ConstraintPropagation.propagateConstraints(pure).map(_.ordered)

  /**
    * Returns all equivalence classes that are explicitly stored in this closure. (I.e., no trivial classes)
    *
    * @return Set of all equivalence classes
    */
  override def classes: Set[Set[Var]] = mapToClasses.values.toSet

}

