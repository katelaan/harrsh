package at.forsyte.harrsh.entailment
import at.forsyte.harrsh.heapautomata.utils.{Closure, ClosureOfAtomSet}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{NullPtr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * A model checker that greedily unfolds the start predicate, matching the obtained pointers against pointers in the model, keeping all candidate unfoldings.
  * This is often a good strategy for data-structure SIDs, where this is generally an almost linear process.
  * (Since these SIDs often have one base case and one recursive case, and generally only one of these applies when we try to match any given model pointer.)
  *
  * Assumption: Garbage-free SID. (Otherwise, matchAllUnfoldedPointers will fail)
  */
object GreedyUnfoldingModelChecker extends SymbolicHeapModelChecker with HarrshLogging {
  // TODO Possibly make this a class with sid in constructor? (To avoid passing around headsToBodies everywhere)
  // TODO Due to the mutual recursion (and thus impossibility to add tailrecursion annotations), stack overflows are likely for large models
  // TODO The linear structure of the set of model pointers is super inefficient, should use a map representation (keys = from vars) instead
  // TODO Heuristics for unfolding process, e.g. based on size, but also via checking which params are nullable in given rule sets

  val IsModel = true
  val NoModel = false

  override def isModel(model: Model, formula : SymbolicHeap, sid: SID) = isModel(model, formula, sid, false)

  def isModel(model: Model, formula : SymbolicHeap, sid: SID, reportProgress: Boolean): Boolean = {

    val modelFormula = ModelToFormula(model)
    val map = sid.rulesAsHeadToBodyMap

    new GreedyUnfolding(map, reportProgress).run(modelFormula, formula, MCHistory.emptyHistory)
  }

  /**
    * Solve the reduced entailment problem via greedy pointer matching. Sound for well-determined heaps?
    * TODO Also sound for some other heaps?
    * @param lhs Reduced symbolic heap on the lhs of the entailment
    * @param rhs Arbitrary symbolic heap on the rhs of the entailment
    * @param sid Underlying SID
    * @return true iff lhs |= rhs
    */
  def reducedEntailmentAsModelChecking(lhs : SymbolicHeap, rhs : SymbolicHeap, sid : SID, reportProgress: Boolean = false): Boolean = {
    val res = new GreedyUnfolding(sid.rulesAsHeadToBodyMap, reportProgress).run(lhs, rhs, MCHistory.emptyHistory)
    if (reportProgress) println("    REDENT result: " + res)
    res
  }

  private class GreedyUnfolding(headsToBodies: Map[String, Set[SymbolicHeap]], reportProgress : Boolean) {

    def run(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, prevHistory: MCHistory): Boolean = {
      val history = prevHistory.nextIteration
      logger.debug("#" * 80)
      logger.debug("Iteration " + history.iteration + ": Greedy model checking of \n     " + formulaToMatch + "\n |?= " + partialUnfolding + "\n}")
      logger.debug("Steps so far:\n" + history.stepLogger)
      if (reportProgress) println("    MC #" + history.iteration + ": " + formulaToMatch + " |?= " + partialUnfolding)
      // There actually shouldn't be a problem with additionally FVs on the lhs --
//      if (formulaToMatch.numFV > partialUnfolding.numFV) {
//        // We should enforce the same number of FVs, but formula instantiation apparently does not currently deal correctly with numFV
//        throw new IllegalStateException("Greedy model checker can only deal with tight models, but (intermediate) model has " + formulaToMatch.numFV + " free variables, (intermediate) unfolding has " + partialUnfolding.numFV)
//      }

      // The entailment/modelchecking result is trivially false if we have too few FVs on the left. View this as error on the caller's part
      if (formulaToMatch.numFV < partialUnfolding.numFV) {
        throw new IllegalStateException("Trivially false MC result: (Intermediate) model " + formulaToMatch + " has " + formulaToMatch.numFV + " free variables, (intermediate) unfolding " + partialUnfolding + " has " + partialUnfolding.numFV)
      }

      val res = if (!formulaToMatch.hasPointer) {
        // No pointers left => Solve the model checking problem for empty models
        logger.debug("Remaining Model " + formulaToMatch + " is now empty")
        checkEmptyModel(formulaToMatch, partialUnfolding, history)
      } else {
        if (partialUnfolding.hasPointer) {
          // Match pair of pointers that are already there
          logger.debug("Unfolding has pointer, will try to match")
          matchUnfoldedPointer(formulaToMatch, partialUnfolding, history)
        } else if (partialUnfolding.hasPredCalls) {
          // If none, unfold to (possibly) get pointers
          logger.debug("Unfolding has no pointer but predicate call, will unfold")
          unfoldAndRecurse(formulaToMatch, partialUnfolding, history)
        } else {
          // Unfolding empty, model non-empty => model checking failed
          logger.debug("Model-checking non-empty model against empty candidate unfolding failed, aborting branch")
          NoModel
        }
      }
      logger.debug("Returning from iteration " + history.iteration)
      res
    }

    private def checkEmptyModel(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, history: MCHistory): Boolean = {
      if (partialUnfolding.hasPointer) {
        // Have matched everything in the model, but there is still a pointer left in the unfolding => not a model
        logger.debug("...but unfolding has pointer => no model, aborting branch")
        NoModel
      } else if (partialUnfolding.hasPredCalls) {
        // Try to replace predicate calls with empty heaps if possible; if so, recurse; otherwise return false
        logger.debug("...but unfolding has calls => generating unfoldings with empty spatial part (if any)")
        val unfoldings = unfoldCallsByEmpty(partialUnfolding)
        logger.debug("Found the following " + unfoldings.size + " empty unfoldings:\n" + unfoldings.mkString("\n"))
        // Depth-first recursion (on arbitrary ordering!) of the models
        lazyRecursiveGreedyUnfolding(formulaToMatch, partialUnfolding, unfoldings, history)
      } else {
        // Return true iff pure constraints of partial unfolding are met
        val res = pureFormulaEntailment(formulaToMatch.pure, partialUnfolding.pure, history)
        logger.debug("Pure entailment check: " + res + " => " + (if (res) " is model" else " no model, abort branch"))
        res
      }
    }

    /**
      * Replaces all remaining calls in sh with rule bodies with empty spatial parts. If no such bodies exist, an empty set is returned.
      * @param sh Arbitrary symbolic heap
      * @return Set of all possible instantiations of calls with empty spatial part
      */
    private def unfoldCallsByEmpty(sh: SymbolicHeap): Set[SymbolicHeap] = {
      if (sh.hasPredCalls) {
        unfoldFirstCallWithSatisfyingBodies(sh, body => !body.hasPointer && !body.hasPredCalls)
      } else {
        Set(sh)
      }
    }

    private def matchUnfoldedPointer(modelFormula: SymbolicHeap, partialUnfolding: SymbolicHeap, history: MCHistory): Boolean = {
      val ptrs: Seq[PointsTo] = partialUnfolding.pointers

      val lhsPtr = try {
        ptrs.find(p => Var.isFV(p.from.getVarOrZero)).get
      } catch {
        case _: NoSuchElementException => throw new Throwable("The selected model-checking algorithm supports only garbage-free SIDs, but your SID appears to have garbage (Pointers unreachable from free variables)")
      }
      logger.debug("Will match pointer " + lhsPtr)

      if (areEqualModuloPureSameSide(partialUnfolding.pure, lhsPtr.from.getVarOrZero, NullPtr().getVarOrZero)) {
        logger.debug("Null pointer on the lhs => Unsatisfiable unfolding => abort branch")
        NoModel
      } else {
        // Take equalities into account while looking for a matching pointer
        //val oMatchingPtr = modelFormula.pointers.find(ptr => areEqualModuloPure(modelFormula.pure, lhsPtr.from.getVarOrZero, ptr.fromAsVar))
        val oMatchingPtr = modelFormula.pointers.find(rhsPtr => areEqualModuloPureDifferentSides(modelFormula.pure, lhsPtr.from.getVarOrZero, partialUnfolding.pure, rhsPtr.fromAsVar))
        oMatchingPtr match {
          case None =>
            logger.debug("No matching pointer => no model => abort branch")
            false
          case Some(matchingPtr) =>
            logger.debug("Found pointer with matching lhs: " + matchingPtr)

            if (lhsPtr.to.size != matchingPtr.to.size) {
              // Pointers have different arity => Not model of the formula
              logger.debug("Matching pointers have different number of args => no model => abort branch")
              NoModel
            }
            else {
              // Found a pointer that matches lhs and length of rhs => perform the acutal matching
              applyParamMatchingToHeaps(modelFormula, partialUnfolding, history, headsToBodies, lhsPtr, matchingPtr)
            }
        }
      }
    }

    def applyParamMatchingToHeaps(modelFormula: SymbolicHeap, partialUnfolding: SymbolicHeap, history: MCHistory, headsToBodies: Map[String, Set[SymbolicHeap]], freePtr: PointsTo, matchingPtr: PointsTo): Boolean = {
      // TODO This is also needlessly inefficient; should get rid of the pointer immediately when we find it to avoid second iteration over the seq
      val smallerUnfolding = partialUnfolding.copy(pointers = partialUnfolding.pointers.filterNot(_ == freePtr))
      val smallerModel = modelFormula.copy(pointers = modelFormula.pointers.filterNot(_ == matchingPtr))
      // Add allocation to history for later comparison
      val newHistory = history.addAlloc(matchingPtr.fromAsVar)

      // Instantiate existentially quantified variables on both sides as necessary
      logger.debug("Will match parameter lists " + matchingPtr.toAsVarOrZero.mkString(",") + " and " + freePtr.toAsVarOrZero.mkString(","))
      renameToMatchParameters(smallerModel, matchingPtr.toAsVarOrZero, smallerUnfolding, freePtr.toAsVarOrZero, newHistory) match {
        case Some((renamedModel, renamedUnfolding)) =>

          logger.debug("New lhs: " + renamedModel)
          logger.debug("New rhs: " + renamedUnfolding)

          // Check if we now have null pointers on the lhs in the model; if so, abort
          if (renamedModel.pointers.exists(_.from.getVarOrZero == 0)) {
            logger.debug("Introduced null pointer allocation into model " + renamedModel + " => no model => abort branch")
            NoModel
          } else {
            // Recurse for continued model checking
            run(renamedModel, renamedUnfolding, newHistory.logStep(MCHistory.PointerMatch(modelFormula, partialUnfolding, freePtr, matchingPtr)))
          }
        case None =>
          // Matching parameters failed, formula is not a model of the unfolding
          logger.debug("Matching pointers have unmatchable args => no model => abort branch")
          NoModel
      }
    }

    @tailrec private def renameToMatchParameters(lhs: SymbolicHeap, lhsParams: Seq[Var], rhs: SymbolicHeap, rhsParams: Seq[Var], history: MCHistory, maxIntroducedFV: Int = 0): Option[(SymbolicHeap, SymbolicHeap)] = {
      // We exploit that both sequences have the same length
      assert(lhsParams.size == rhsParams.size)
      //logger.debug("Renaming " + lhs + " / " + rhs + " to match " + lhsParams.mkString(", ") + " / " + rhsParams.mkString(", "))

      def unbindBoundVariable(lvar: Var, rvar: Var): (SymbolicHeap, SymbolicHeap, Int) = {
        // The new FV will be one larger than any of the FVs in either argument SH and the history
        // Note that if the arguments SHs have different numbers of FVs, this will introduce a "gap" in the SH with the smaller number
        val usedFVIdents = Seq(maxIntroducedFV, lhs.numFV, rhs.numFV) ++ history.alloc
        val newFV = Var.mkVar(usedFVIdents.max + 1)
        logger.debug("Introducing new free variable " + PtrVar(newFV) + " replacing " + lvar + " (model formula) and " + rvar + " (unfolding)")
        val newLhs = lhs.instantiateBoundVar(lvar, newFV)
        val newRhs = rhs.instantiateBoundVar(rvar, newFV)
        (newLhs, newRhs, newFV)
      }

      if (lhsParams.isEmpty) {
        Some(lhs, rhs)
      } else {
        val (lvar, rvar) = (lhsParams.head, rhsParams.head)
        if (lvar == rvar) {
          // If both variables are quantified, we need to make sure they are interpreted the same on both sides by forcing instantiation with the same FV
          val (newLhs, newRhs, newFV) = if (Var.isFV(lvar)) (lhs, rhs, maxIntroducedFV) else unbindBoundVariable(lvar, rvar)
          renameToMatchParameters(newLhs, lhsParams.tail, newRhs, rhsParams.tail, history, newFV)
        } else {
          (Var.isFV(lvar), Var.isFV(rvar)) match {
            case (true, true) =>
              // The vars are different FVs => Need to be equal under pure in the model...
              if (areEqualModuloPureDifferentSides(lhs.pure, lvar, rhs.pure, rvar)) {
                // ...in which case we don't need to do any renaming...
                renameToMatchParameters(lhs, lhsParams.tail, rhs, rhsParams.tail, history, maxIntroducedFV)
              } else {
                // ...but if they are not the same in the candidate model, it's not a model of rhs
                None
              }
            case (true, false) =>
              // Left is free, right is quantified => Instantiate right-hand side
              logger.debug("Renaming " + rvar + " to " + lvar + " in unfolding")
              val newRhs = rhs.instantiateBoundVar(rvar, lvar)
              renameToMatchParameters(lhs, lhsParams.tail, newRhs, rhsParams.tail, history, maxIntroducedFV)
            case (false, true) =>
              // Left is quantified, right is free => Instantiate left-hand side
              logger.debug("Renaming " + lvar + " to " + rvar + " in model formula")
              val newLhs = lhs.instantiateBoundVar(lvar, rvar)
              renameToMatchParameters(newLhs, lhsParams.tail, rhs, rhsParams.tail, history, maxIntroducedFV)
            case (false, false) =>
              // Both are bound; need to introduce new free var to continue matching
              val (newLhs, newRhs, newFV) = unbindBoundVariable(lvar, rvar)
              renameToMatchParameters(newLhs, lhsParams.tail, newRhs, rhsParams.tail, history, newFV)
          }
        }
      }
    }

    private def unfoldAndRecurse(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, history: MCHistory): Boolean = {
      // Arbitrarily pick first call
      val heaps = unfoldFirstCallWithSatisfyingBodies(partialUnfolding, _ => true)
      lazyRecursiveGreedyUnfolding(formulaToMatch, partialUnfolding, heaps, history)
    }

    private def unfoldFirstCallWithSatisfyingBodies(sh: SymbolicHeap, pBody: SymbolicHeap => Boolean): Set[SymbolicHeap] = {
      val call = sh.predCalls.head
      val applicableBodies = headsToBodies(call.name) filter (pBody)
      logger.debug("Will unfold " + call + " by...\n" + applicableBodies.map("  - " + _).mkString("\n"))
      val unfolded = for (body <- applicableBodies) yield sh.replaceCall(call, body, performAlphaConversion = false)
      unfolded
    }

    /**
      * Depth-first recursion, using the candidateUnfoldings as new right-hand sides, stopping as soon as one computation path succeeds
      */
    private def lazyRecursiveGreedyUnfolding(modelFormula: SymbolicHeap, partialUnfolding: SymbolicHeap, candidateUnfoldings: Set[SymbolicHeap], history: MCHistory): Boolean = {
      // Depth-first traversal of candidate unfoldings
      // TODO Better heuristics, e.g. based on #ptrs in base rule and branching factor?
      Combinators.lazyAny(candidateUnfoldings.toSeq.sortWith(_.predCalls.size > _.predCalls.size),
        (sh: SymbolicHeap) => run(modelFormula, sh, history.logStep(MCHistory.UnfoldingStep(modelFormula, partialUnfolding, sh))))
    }

    private def areEqualModuloPureSameSide(pure: Seq[PureAtom], fst: Var, snd: Var): Boolean = {
      logger.debug("Will check equality " + fst + " = " + snd + " modulo "+ pure)
      if (fst == snd) {
        // Syntactically the same => trivially equal
        true
      } else {
        // Not the same var => Have to check equalities
        val closure: Closure = new ClosureOfAtomSet(pure.toSet)
        closure.getEqualityClass(fst).contains(snd)
      }
    }

    private def areEqualModuloPureDifferentSides(pureLhs: Seq[PureAtom], lhs: Var, pureRhs: Seq[PureAtom], rhs: Var): Boolean = {
      val msg = "Will check equality " + lhs + "%[" + pureLhs.mkString(",") + "] = " + rhs + "%[" + pureRhs.mkString(",") + "]"
      if (lhs == rhs) {
        // Syntactically the same => trivially equal
        true
      } else {
        // Not the same var => Have to check equalities
        val closureLhs: Closure = new ClosureOfAtomSet(pureLhs.toSet)
        val closureRhs: Closure = new ClosureOfAtomSet(pureRhs.toSet)
        val res = closureRhs.getEqualityClass(rhs).exists(closureLhs.getEqualityClass(lhs))
        if (res) {
          logger.debug(msg)
          logger.debug("RHS equality class " + closureRhs.getEqualityClass(rhs) + " contains " + closureRhs.getEqualityClass(rhs).find(closureLhs.getEqualityClass(lhs)) + " from LHS equality class " + closureLhs.getEqualityClass(lhs) + " => successful match")
        } else {
          logger.debug(msg + "... Equality check failed")
        }
        res
      }
    }

    // FIXME Is this the correct entailment check?
    private def pureFormulaEntailment(lhs: Seq[PureAtom], rhs: Seq[PureAtom], history: MCHistory): Boolean = {
      // Add pure formulas according to history
      val historyConstraint = history.toFormula
      val lhsPure = lhs ++ historyConstraint
      logger.debug("Checking pure entailment\n     " + lhsPure.mkString(", ") + "\n |?= " + rhs.mkString(", "))
      if (reportProgress) println("    Checking pure entailment " + lhsPure.mkString(", ") + " |?= " + rhs.mkString(", "))

      val lhsClosure: Closure = new ClosureOfAtomSet(lhsPure.toSet)
      val rhsClosure: Closure = new ClosureOfAtomSet(rhs.toSet)

      // TODO Actually there is no need to compute the closure explicitly, should improve this at some point
      // Every equality and disequality in the rhs should be respected by the lhs, apart from explicit equalities of the kind x == x
      def notTautology(atom: PureAtom) = atom.isInstanceOf[PtrNEq] || atom.getVarsWithNull.size == 2

      val lhsClosureSet = lhsClosure.asSetOfAtoms filter notTautology
      val rhsClosureSet = rhsClosure.asSetOfAtoms filter notTautology

      // After we've computed the closure, we can throw away existentially quantified variables:
      // The (existentially!) quantified variables do not strengthen the constraints in any way.
      val lhsFreeClosureSet = lhsClosureSet filterNot (atom => atom.getVars.exists(Var.isBound(_)))
      val rhsFreeClosureSet = rhsClosureSet filterNot (atom => atom.getVars.exists(Var.isBound(_)))

      logger.debug(rhsFreeClosureSet + " subset of " + lhsFreeClosureSet + "?")
      rhsFreeClosureSet subsetOf lhsFreeClosureSet
    }

  }

}
