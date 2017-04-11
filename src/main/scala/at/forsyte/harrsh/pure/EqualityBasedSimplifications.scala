package at.forsyte.harrsh.pure

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{Renaming, Var}
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PtrNEq, PureAtom, SymbolicHeap}

/**
  * Created by jkatelaa on 3/31/17.
  */
object EqualityBasedSimplifications extends HarrshLogging {

  /**
    * Removes all bound variables y from sh for which sh contains an explicit equation x = y or y = x, but does not compute the congruence closure to discover equalities that are consequences of sh's pure constraints.
    * @param sh The symbolic heap to simplify
    * @return The simplified symbolic heaps
    */
  def removeExplicitlyRedundantBoundVars(sh : SymbolicHeap) : SymbolicHeap = {
    val mixedEqs : Seq[(Var,Var)] = sh.pure.flatMap(asEqualityWithBoundPart)
    // TODO Note that this is not optimal, because of cases such as y2 = y3, y1 = y2. In that case, we could get rid of two variables, but would only remove one here (depending on the order of the equalities)
    val renaming = Renaming.fromPairs(mixedEqs)
    logger.debug("Renaming " + sh + " using " + renaming)
    val renamed = sh.renameVars(renaming, avoidDoubleCapture = false)
    logger.debug("Renamed: " + renamed)
    removeTautologies(renamed)
  }

  private def asEqualityWithBoundPart(atom : PureAtom) : Option[(Var,Var)] = atom match {
    case PtrEq(l, r) =>
      val lFree = Var.isFV(l.getVarOrZero)
      val rFree = Var.isFV(r.getVarOrZero)
      // If one of the vars is free, we replace the bound var by the free var...
      if (lFree && !rFree) Some(r.getVarOrZero, l.getVarOrZero)
      else if (rFree && !lFree) Some(l.getVarOrZero, r.getVarOrZero)
      // whereas if both are unfree, we can simply get rid of one bound var...
      else if (!lFree && !rFree) Some(l.getVarOrZero, r.getVarOrZero)
      // ...but if both are free, we cannot do anything, because we do not want to influence the arity of the heap
      else None
    case PtrNEq(l, r) => None
  }

  /**
    * Removes all explicit tautologies from the given symbolic heap, i.e., equalities that are directly contained directly in the pure constraints;
    * does not remove tautologies that can only be discovered through transitive reasoning.
    * @param sh The symbolic heap to clean up
    * @return sh without explicit tautologies
    */
  def removeTautologies(sh : SymbolicHeap) : SymbolicHeap = {
    sh.copy(pure = sh.pure.filterNot(isTautology))
  }

  private def isTautology(atom : PureAtom) : Boolean = atom match {
    case PtrEq(l, r) => l == r
    case PtrNEq(l, r) => false
  }

}
