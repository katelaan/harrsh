package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{NullPtr, PtrVar, Var, _}
import at.forsyte.harrsh.seplog.inductive.{PointsTo, SymbolicHeap}

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 3/24/17.
  */
object PointerUnification extends HarrshLogging {

  case class PointerMatch(lhsPtr : PointsTo, rhsPtr : PointsTo, newFVs : Set[Var], newLhs : SymbolicHeap, newRhs : SymbolicHeap)

  /**
    * Finds and removes a unifiable pointer pair in the given pair of formulas
    * @param lhsFormula Formula to unify with rhsFormula
    * @param rhsFormula Formula to unify with lhsFormula
    * @param disallowedFVs Variable identifiers that must not be introduced in the unification
    * @return Some unifiable pointer pair + result of unification (with matching pair removed) or nothing
    */
  def removeUnifiablePointerPair(lhsFormula: SymbolicHeap, rhsFormula: SymbolicHeap, disallowedFVs : Set[Var]): Option[PointerMatch] = {
    val rhsPtr = try {
      rhsFormula.pointers.find(p => Var.isFV(p.from.getVarOrZero)).get
    } catch {
      case _: NoSuchElementException => throw new Throwable("The selected model-checking algorithm supports only garbage-free SIDs, but your SID appears to have garbage (Pointers unreachable from free variables)")
    }
    logger.debug("Will match pointer " + rhsPtr)

    if (PureFormulaReasoning.varsEqualModuloPureSameSide(rhsFormula.pure, rhsPtr.from.getVarOrZero, NullPtr().getVarOrZero)) {
      logger.debug("Null pointer on the left side of pointer => Unsatisfiable unfolding => abort branch")
      None
    } else {
      val oMatchingLhsPtr = findPointerWithMatchingLeftSide(lhsFormula, rhsFormula, rhsPtr)
      oMatchingLhsPtr match {
        case None =>
          logger.debug("No matching pointer => no model => abort branch")
          None
        case Some(matchingLhsPtr) =>
          // Found a pointer that matches lhs and length of rhs => perform the acutal matching
          // TODO This is also needlessly inefficient; should get rid of the pointer immediately when we find it to avoid second iteration over the seq
          val smallerLhs = lhsFormula.copy(pointers = lhsFormula.pointers.filterNot(_ == matchingLhsPtr))
          val smallerRhs = rhsFormula.copy(pointers = rhsFormula.pointers.filterNot(_ == rhsPtr))
          applyParamMatchingToHeaps(smallerLhs, smallerRhs, matchingLhsPtr, rhsPtr, disallowedFVs)
      }
    }
  }

  private def findPointerWithMatchingLeftSide(lhsFormula : SymbolicHeap, rhsFormula : SymbolicHeap, rhsPtr : PointsTo ): Option[PointsTo] = {
    // Take equalities into account while looking for a matching pointer
    //val oMatchingLhsPtr = lhs.pointers.find(ptr => areEqualModuloPure(lhs.pure, lhsPtr.from.getVarOrZero, ptr.fromAsVar))
    val oMatchingLhsPtr = lhsFormula.pointers.find(lhsPtr => PureFormulaReasoning.varsEqualModuloPureDifferentSides(lhsFormula.pure, lhsPtr.from.getVarOrZero, rhsFormula.pure, rhsPtr.fromAsVar))
    oMatchingLhsPtr match {
      case None =>
        logger.debug("No matching pointer => no model => abort branch")
        None
      case Some(matchingLhsPtr) =>
        logger.debug("Found pointer with matching left side: " + matchingLhsPtr)

        if (rhsPtr.to.size != matchingLhsPtr.to.size) {
          // Pointers have different arity => Not model of the formula
          logger.debug("Matching pointers have different number of args => no model => abort branch")
          None
        }
        else {
          // Found a pointer that matches lhs and length of rhs => perform the acutal matching
          Some(matchingLhsPtr)
        }
    }
  }

  /**
    * Modifies the given lhs/rhs, introducing new FVs as necessary, while matching the given pointers against each other
    * @param lhs Remaining left-hand side heap (without lhsPtr)
    * @param rhs Remaining right-hand side heap (without rhsPtr)
    * @param lhsPtr Pointer to match against rhsPtr
    * @param rhsPtr Pointer to match against lhsPtr
    * @param disallowedFVs Free variable identifier that must NOT be introduced in parameter matching
    * @return
    */
  private def applyParamMatchingToHeaps(lhs: SymbolicHeap, rhs: SymbolicHeap, lhsPtr: PointsTo, rhsPtr: PointsTo, disallowedFVs : Set[Var]): Option[PointerMatch] = {

    // Because the pointer is allocated, its left side must not be introduced as fresh free variable later
    val newAllocatedVar = rhsPtr.fromAsVar

    // Instantiate existentially quantified variables on both sides as necessary
    logger.debug("Will match parameter lists " + rhsPtr.toAsVarOrZero.mkString(",") + " and " + lhsPtr.toAsVarOrZero.mkString(","))
    matchParametersViaVariableIntroduction(lhs, lhsPtr.toAsVarOrZero, rhs, rhsPtr.toAsVarOrZero, disallowedFVs, Set(newAllocatedVar)) match {
      case Some((renamedLhs, renamedRhs, newFVs)) =>

        logger.debug("New lhs: " + renamedLhs)
        logger.debug("New rhs: " + renamedRhs)

        // Check if we now have null pointers on the lhs in the model; if so, abort
        if (renamedLhs.pointers.exists(_.from.getVarOrZero == 0)) {
          logger.debug("Introduced null pointer allocation into model " + renamedLhs + " => no model => abort branch")
          None
        } else {
          // Recurse for continued model checking
          Some(PointerMatch(lhsPtr, rhsPtr, newFVs, renamedLhs, renamedRhs))
        }
      case None =>
        // Matching parameters failed, formula is not a model of the unfolding
        logger.debug("Matching pointers have unmatchable args => no model => abort branch")
        None
    }
  }

  @tailrec private def matchParametersViaVariableIntroduction(lhs: SymbolicHeap, lhsParams: Seq[Var], rhs: SymbolicHeap, rhsParams: Seq[Var], disallowedFVs : Set[Var], newDisallowedFVs : Set[Var]): Option[(SymbolicHeap, SymbolicHeap, Set[Var])] = {
    // We exploit that both sequences have the same length
    assert(lhsParams.size == rhsParams.size)
    //logger.debug("Renaming " + lhs + " / " + rhs + " to match " + lhsParams.mkString(", ") + " / " + rhsParams.mkString(", "))

    if (lhsParams.isEmpty) {
      Some(lhs, rhs, newDisallowedFVs)
    } else {
      val (lvar, rvar) = (lhsParams.head, rhsParams.head)
      matchParameterPair(lhs, rhs, lvar, rvar, disallowedFVs ++ newDisallowedFVs) match {
        case None => None
        case Some((newLhs, newRhs, newFV)) => matchParametersViaVariableIntroduction(newLhs, lhsParams.tail, newRhs, rhsParams.tail, disallowedFVs, newDisallowedFVs ++ newFV)
      }
    }
  }

  /**
    * Matches a single parameter pair, introducing a new free variable if necessary, in which case the symbolic heaps are renamed accordingly
    */
  def matchParameterPair(lhs : SymbolicHeap, rhs : SymbolicHeap, lvar : Var, rvar : Var, allDisallowedFVs : Set[Var]) : Option[(SymbolicHeap, SymbolicHeap, Option[Var])] = {

    def unbindBoundVariable(lvar: Var, rvar: Var): (SymbolicHeap, SymbolicHeap, Option[Var]) = {
      // The new FV will be one larger than any of the FVs in either argument SH and the history
      // Note that if the arguments SHs have different numbers of FVs, this will introduce a "gap" in the SH with the smaller number
      val usedFVIdents = Seq(allDisallowedFVs.max, lhs.numFV, rhs.numFV)
      val newFV = Var.mkVar(usedFVIdents.max + 1)
      logger.debug("Introducing new free variable " + PtrVar(newFV) + " replacing " + lvar + " (model formula) and " + rvar + " (unfolding)")
      val newLhs = lhs.instantiateBoundVar(lvar, newFV)
      val newRhs = rhs.instantiateBoundVar(rvar, newFV)
      (newLhs, newRhs, Some(newFV))
    }

    if (lvar == rvar) {
      if (Var.isFV(lvar)) {
        // Already same free vars => Nothing to do
        Some(lhs, rhs, None)
      } else {
        // If both variables are quantified, we need to make sure they are interpreted the same on both sides by forcing instantiation with the same FV
        Some(unbindBoundVariable(lvar, rvar))
      }
    } else {
      (Var.isFV(lvar), Var.isFV(rvar)) match {
        case (true, true) =>
          // The vars are different FVs => Need to be equal under pure in the model...
          if (PureFormulaReasoning.varsEqualModuloPureDifferentSides(lhs.pure, lvar, rhs.pure, rvar)) {
            // ...in which case we don't need to do any renaming...
            Some(lhs, rhs, None)
          } else {
            // ...but if they are not the same in the candidate model, it's not a model of rhs
            None
          }
        case (true, false) =>
          // Left is free, right is quantified => Instantiate right-hand side
          logger.debug("Renaming " + rvar + " to " + lvar + " in unfolding")
          val newRhs = rhs.instantiateBoundVar(rvar, lvar)
          Some(lhs, newRhs, None)
        case (false, true) =>
          // Left is quantified, right is free => Instantiate left-hand side
          logger.debug("Renaming " + lvar + " to " + rvar + " in model formula")
          val newLhs = lhs.instantiateBoundVar(lvar, rvar)
          Some(newLhs, rhs, None)
        case (false, false) =>
          // Both are bound; need to introduce new free var to continue matching
          Some(unbindBoundVariable(lvar, rvar))
      }
    }
  }

}
