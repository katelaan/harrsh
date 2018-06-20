package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityUtils
import at.forsyte.harrsh.seplog.{NullPtr, PtrVar, Var, _}
import at.forsyte.harrsh.seplog.inductive.{PointsTo, SymbolicHeap}

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 3/24/17.
  */
object PointerUnification extends HarrshLogging {

  // TODO: Extend from garbage-free heaps to "undirected garbage freedom", i.e. every bound variable can be reached or can reach a free var. This would be a useful generalization in particular for equivalence checking, but also model checking

  case class PointerMatch(lhsPtr : PointsTo, rhsPtr : PointsTo, newlyAllocatedFV : Var, newLhs : SymbolicHeap, newRhs : SymbolicHeap)

  /**
    * Finds and removes a unifiable pointer pair in the given pair of formulas
    * @param lhsFormula Formula to unify with rhsFormula
    * @param rhsFormula Formula to unify with lhsFormula
    * @param disallowedFVs Variable identifiers that must not be introduced in the unification
    * @return Some unifiable pointer pair + result of unification (with matching pair removed) or nothing
    */
  def removeUnifiablePointerPair(lhsFormula: SymbolicHeap, rhsFormula: SymbolicHeap, disallowedFVs : Set[Var]): Option[PointerMatch] = {
    val rhsPtr = try {
      rhsFormula.pointers.find(p => p.from.getVarOrZero.isFree).get
    } catch {
      case _: NoSuchElementException =>
        logger.warn("Pointer unification for garbage-free heaps was fed heap with garbage: " + rhsFormula)
        throw new Throwable("The selected algorithm supports only garbage-free heaps, but has encountered garbage (Pointers unreachable from free variables)")
    }
    logger.debug("Will match pointer " + rhsPtr)

    if (EqualityUtils.varsEqualModuloPureSameSide(rhsFormula.pure, rhsPtr.from.getVarOrZero, NullPtr.getVarOrZero)) {
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
    val oMatchingLhsPtr = lhsFormula.pointers.find(lhsPtr => EqualityUtils.varsEqualModuloPureDifferentSides(lhsFormula.pure, lhsPtr.from.getVarOrZero, rhsFormula.pure, rhsPtr.fromAsVar))
    oMatchingLhsPtr match {
      case None =>
        logger.debug("No matching pointer => no model => abort branch")
        None
      case Some(matchingLhsPtr) =>
        logger.trace("Found pointer with matching left side: " + matchingLhsPtr)

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
    val rhsVarToAllocate = rhsPtr.fromAsVar

    // Instantiate existentially quantified variables on both sides as necessary
    logger.trace("Will match parameter lists " + rhsPtr.toAsVarOrZero.mkString(",") + " and " + lhsPtr.toAsVarOrZero.mkString(","))
    matchParametersViaVariableIntroduction(lhs, lhsPtr.toAsVarOrZero, rhs, rhsPtr.toAsVarOrZero, disallowedFVs, Set(rhsVarToAllocate), Seq.empty) match {
      case Some((renamedLhs, renamedRhs)) =>

        logger.debug("New lhs: " + renamedLhs)
        logger.debug("New rhs: " + renamedRhs)

        // Check if we now have null pointers on the lhs in the model; if so, abort
        if (renamedLhs.pointers.exists(_.from.getVarOrZero == Var(0))) {
          logger.debug("Introduced null pointer allocation into model " + renamedLhs + " => no model => abort branch")
          None
        } else {
          Some(PointerMatch(lhsPtr, rhsPtr, rhsVarToAllocate, renamedLhs, renamedRhs))
        }
      case None =>
        // Matching parameters failed, formula is not a model of the unfolding
        logger.debug("Matching pointers have unmatchable args => no model => abort branch")
        None
    }
  }

  // TODO Get rid of partialParameterMatching? Currently only used in debugging
  @tailrec private def matchParametersViaVariableIntroduction(lhs: SymbolicHeap, lhsParams: Seq[Var], rhs: SymbolicHeap, rhsParams: Seq[Var], disallowedFVs : Set[Var], newDisallowedFVs : Set[Var], partialParameterMatching : Seq[Var]): Option[(SymbolicHeap, SymbolicHeap)] = {
    // We exploit that both sequences have the same length
    assert(lhsParams.size == rhsParams.size)
    //logger.trace("Renaming " + lhs + " / " + rhs + " to match " + lhsParams.mkString(", ") + " / " + rhsParams.mkString(", "))

    if (lhsParams.isEmpty) {
      logger.debug("Final parameter matching: " + partialParameterMatching.mkString(", "))
      Some(lhs, rhs)
    } else {
      val (lvar, rvar) = (lhsParams.head, rhsParams.head)
      matchParameterPair(lhs, rhs, lvar, rvar, disallowedFVs ++ newDisallowedFVs) match {
        case None => None
        case Some((newLhs, newRhs, matchingResult, newFV)) => matchParametersViaVariableIntroduction(newLhs, lhsParams.tail, newRhs, rhsParams.tail, disallowedFVs, newDisallowedFVs ++ newFV, partialParameterMatching :+ matchingResult)
      }
    }
  }

  /**
    * Matches a single parameter pair, introducing a new free variable if necessary, in which case the symbolic heaps are renamed accordingly
    */
  def matchParameterPair(lhs : SymbolicHeap, rhs : SymbolicHeap, lvar : Var, rvar : Var, allDisallowedFVs : Set[Var]) : Option[(SymbolicHeap, SymbolicHeap, Var, Option[Var])] = {

    def unbindBoundVariable(lvar: Var, rvar: Var): (SymbolicHeap, SymbolicHeap, Var, Option[Var]) = {
      // The new FV will be one larger than any of the FVs in either argument SH and the history
      // Note that if the arguments SHs have different numbers of FVs, this will introduce a "gap" in the FVs in the SH with the smaller number
      val usedFVIdents = Seq(Var.maxOf(allDisallowedFVs).toInt, lhs.numFV, rhs.numFV)
      val newFV = Var(usedFVIdents.max + 1)
      logger.trace("Introducing new free variable " + PtrVar(newFV) + " replacing " + lvar + " (model formula) and " + rvar + " (unfolding)")
      val newLhs = lhs.instantiateBoundVars(Seq((lvar, newFV)), closeGaps = false)
      val newRhs = rhs.instantiateBoundVars(Seq((rvar, newFV)), closeGaps = false)
      (newLhs, newRhs, newFV, Some(newFV))
    }

    if (lvar == rvar) {
      if (lvar.isFree) {
        // Already same free vars => Nothing to do
        Some(lhs, rhs, lvar, None)
      } else {
        // If both variables are quantified, we need to make sure they are interpreted the same on both sides by forcing instantiation with the same FV
        Some(unbindBoundVariable(lvar, rvar))
      }
    } else {
      (lvar.isFree, rvar.isFree) match {
        case (true, true) =>
          // The vars are different FVs => Need to be equal under pure in the model...
          if (EqualityUtils.varsEqualModuloPureDifferentSides(lhs.pure, lvar, rhs.pure, rvar)) {
            // ...in which case we don't need to do any renaming...
            Some(lhs, rhs, lvar, None)
          } else {
            // ...but if they are not the same in the candidate model, it's not a model of rhs
            None
          }
        case (true, false) =>
          // Left is free, right is quantified => Instantiate right-hand side
          logger.trace("Renaming " + rvar + " to " + lvar + " in unfolding")
          val newRhs = rhs.instantiateBoundVars(Seq((rvar, lvar)), closeGaps = false)
          Some(lhs, newRhs, lvar, None)
        case (false, true) =>
          // Left is quantified, right is free => Instantiate left-hand side
          logger.trace("Renaming " + lvar + " to " + rvar + " in model formula")
          val newLhs = lhs.instantiateBoundVars(Seq((lvar, rvar)), closeGaps = false)
          Some(newLhs, rhs, rvar, None)
        case (false, false) =>
          // Both are bound; need to introduce new free var to continue matching
          Some(unbindBoundVariable(lvar, rvar))
      }
    }
  }

}
