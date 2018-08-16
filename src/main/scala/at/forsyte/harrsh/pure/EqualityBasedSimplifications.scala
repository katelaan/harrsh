package at.forsyte.harrsh.pure

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{Renaming, Var}
import at.forsyte.harrsh.seplog.inductive._

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 3/31/17.
  */
object EqualityBasedSimplifications extends HarrshLogging {

  def fullEqualitySimplification(sh : SymbolicHeap) : SymbolicHeap = {
    SymbolicHeap(fullEqualitySimplification(sh.atoms), sh.freeVars)
  }

  @tailrec private def fullEqualitySimplification(atoms : AtomContainer) : AtomContainer = {
    val withoutRedundancies = removeExplicitlyRedundantBoundVars(atoms)
    if (withoutRedundancies != atoms)
      fullEqualitySimplification(withoutRedundancies)
    else
      withoutRedundancies.closeGapsInBoundVars
  }

  /**
    * Removes all bound variables y from sh for which sh contains an explicit equation x = y or y = x,
    * but does not compute the congruence closure to discover equalities that are consequences of sh's pure constraints.
    *
    * Note that this is not optimal, because of cases such as y2 = y3, y1 = y2. In that case, we could get rid of two variables,
    * but would only remove one here (depending on the order of the equalities). [[fullEqualitySimplification()]] also removes
    * such transitive equalities at the expense of higher runtime.
    *
    * @param atoms The atoms to simplify
    * @return The simplified symbolic heaps
    */
  def removeExplicitlyRedundantBoundVars(atoms : AtomContainer) : AtomContainer = {
    val mixedEqs : Seq[(Var,Var)] = atoms.pure.flatMap(asEqualityWithBoundPart)
    val renaming = Renaming.fromPairs(mixedEqs)
    logger.debug("Renaming " + atoms + " using " + renaming)
    val renamed = atoms.rename(renaming, avoidDoubleCapture = false)
    logger.debug("Renamed: " + renamed)
    removeTautologies(renamed)
  }

  private def asEqualityWithBoundPart(atom : PureAtom) : Option[(Var,Var)] = atom match {
    case PureAtom(l, r, true) =>
      val lFree = l.isFree
      val rFree = r.isFree
      // If one of the vars is free, we replace the bound var by the free var...
      if (lFree && !rFree) Some(r, l)
      else if (rFree && !lFree) Some(l, r)
      // whereas if both are unfree, we can simply get rid of one bound var...
      else if (!lFree && !rFree) Some(l, r)
      // ...but if both are free, we cannot do anything, because we do not want to influence the arity of the heap
      else None
    case _ => None
  }

  /**
    * Removes all explicit tautologies from the given symbolic heap, i.e., equalities that are directly contained directly in the pure constraints;
    * does not remove tautologies that can only be discovered through transitive reasoning.
    * @param atoms The set of atoms to clean up
    * @return sh without explicit tautologies
    */
  def removeTautologies(atoms : AtomContainer) : AtomContainer = {
    atoms.copy(pure = atoms.pure.filterNot(isTautology))
  }

  private def isTautology(atom : PureAtom) : Boolean = atom match {
    case PureAtom(l, r, isEquality) => isEquality && l == r
  }

}
