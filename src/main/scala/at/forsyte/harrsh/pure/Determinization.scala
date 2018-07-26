package at.forsyte.harrsh.pure

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PtrNEq, PureAtom, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 5/18/17.
  */
object Determinization extends HarrshLogging {

  def isDetermined(rsh : SymbolicHeap) = undeterminedRelationships(rsh).isEmpty

  // TODO Problem: This naive determinization can produce inconsistent heaps! E.g. for x1 -> x2, one possible determinization is x1 -> x2 : { x1 = x2, x2 = null }!
  def rshDeterminizations(rsh : SymbolicHeap) : Seq[SymbolicHeap] = {
    logger.debug("Determinization of " + rsh)
    val undeterminedPairs = undeterminedRelationships(rsh)
    determineRelationshipsOf(rsh, undeterminedPairs)
  }

  def determineRelationshipsOf(rsh : SymbolicHeap, undeterminedPairs : Seq[(Var,Var)]) : Seq[SymbolicHeap] = {
    if (undeterminedPairs.isEmpty) {
      // Already determined
      Seq(rsh)
    } else {
      // Not determined, create one new RSH per determinization choice
      for {
        determinizationChoice <- determinizationChoices(undeterminedPairs)
      } yield rsh.copy(pure = rsh.pure ++ determinizationChoice)
    }
  }

  private def determinizationChoices(undeterminedPairs : Seq[(Var,Var)]) : Seq[Seq[PureAtom]] = {
    val determiningConstraints: Seq[Set[PureAtom]] = undeterminedPairs map {
      case (fst, snd) => Set[PureAtom](PtrEq(fst, snd), PtrNEq(fst, snd))
    }
    Combinators.choices(determiningConstraints)
  }

  def undeterminedRelationships(rsh : SymbolicHeap) : Seq[(Var,Var)] = {
    val constraints = ConsistencyCheck.symbolicHeapToEqualityConstraintsQuantified(rsh)
    logger.debug("Full set of constraints: " + constraints.mkString("{",",","}"))

    val closure = Closure.ofSetOfAtoms(constraints)
    logger.debug("Closure: " + closure.asSetOfAtoms)

    val vars = rsh.allVars + NullConst
    val relevantVars = closure.classRepresentativesOf(vars).toIndexedSeq
    logger.debug("One variable per equivalence class: " + relevantVars.mkString(", "))
    val numvars = relevantVars.size

    val undeterminedPairs : Seq[(Var,Var)] = for {
      fst <- 0 until numvars - 1
      snd <- fst + 1 until numvars
      if !determinedRelationship(closure, relevantVars(fst), relevantVars(snd))
    } yield {
      // Using ordered pairs for deterministic results, not necessary for correctness
      orderedPair(relevantVars(fst), relevantVars(snd))
    }

    logger.debug("Undetermined relationships: " + undeterminedPairs.mkString(", "))
    undeterminedPairs
  }

  private def determinedRelationship(cl : Closure, fst : Var, snd : Var) : Boolean = {
    // We're only working with one representative of each class, so the following should hold
    assert(cl.getEquivalenceClass(fst) != cl.getEquivalenceClass(snd))

    cl.asSetOfAtoms.exists(isInequalityBetween(fst,snd))
  }

  private def orderedPair(fst : Var, snd : Var) = if (fst < snd) (fst, snd) else (snd, fst)

  private def isInequalityBetween(fst : Var, snd: Var)(atom : PureAtom) : Boolean = atom match {
    case PureAtom(l, r, isEquality) => !isEquality && (l == fst && r == snd) || (l == snd && r == fst)
  }

}
