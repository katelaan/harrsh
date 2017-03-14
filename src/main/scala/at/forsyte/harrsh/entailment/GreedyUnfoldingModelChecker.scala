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
  type History = Set[Var]
  val emptyHistory = Set.empty[Var]

  override def isModel(model: Model, formula : SymbolicHeap, sid: SID): Boolean = {

    val modelFormula = ModelToFormula(model)
    val map = sid.rulesAsHeadToBodyMap

    greedyUnfolding(modelFormula, formula, emptyHistory, map, 1)
  }

  /**
    * Solve the reduced entailment problem via greedy pointer matching. Sound for well-determined heaps?
    * TODO Also sound for some other heaps?
    * @param lhs Reduced symbolic heap on the lhs of the entailment
    * @param rhs Arbitrary symbolic heap on the rhs of the entailment
    * @param sid Underlying SID
    * @return true iff lhs |= rhs
    */
  def reducedEntailmentAsModelChecking(lhs : SymbolicHeap, rhs : SymbolicHeap, sid : SID): Boolean = {
    greedyUnfolding(lhs, rhs, emptyHistory, sid.rulesAsHeadToBodyMap, 1)
  }

  private def greedyUnfolding(formulaToMatch : SymbolicHeap, partialUnfolding : SymbolicHeap, history : History, headsToBodies: Map[String, Set[SymbolicHeap]], iteration : Int) : Boolean = {
    logger.debug("#"*80)
    logger.debug("Iteration " + iteration + ": Greedy model checking of \n     " + formulaToMatch + "\n |?= " + partialUnfolding +"\n}")
    if (formulaToMatch.numFV > partialUnfolding.numFV) {
      // FIXME We should enforce the same number of FVs, but formula instantiation apparently does not currently deal correctly with numFV
      throw new IllegalStateException("Greedy model checker can only deal with tight models, but (intermediate) model has " + formulaToMatch.numFV + " free variables, (intermediate) unfolding has " + partialUnfolding.numFV)
    }

    val res =if (!formulaToMatch.hasPointer) {
      // No pointers left => Solve the model checking problem for empty models
      logger.debug("Remaining Model " + formulaToMatch + " is now empty")
      checkEmptyModel(formulaToMatch, partialUnfolding, history, headsToBodies, iteration)
    } else {
      if (partialUnfolding.hasPointer) {
        // Match pair of pointers that are already there
        logger.debug("Unfolding has pointer, will try to match")
        matchUnfoldedPointer(formulaToMatch, partialUnfolding, history, headsToBodies, iteration)
      } else if (partialUnfolding.hasPredCalls) {
        // If none, unfold to (possibly) get pointers
        logger.debug("Unfolding has no pointer but predicate call, will unfold")
        unfoldAndRecurse(formulaToMatch, partialUnfolding, history, headsToBodies, iteration)
      } else {
        // Unfolding empty, model non-empty => model checking failed
        logger.debug("Model-checking non-empty model against empty candidate unfolding failed, aborting branch")
        NoModel
      }
    }
    logger.debug("Returning from iteration " + iteration)
    res
  }

  private def checkEmptyModel(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, history: History, headsToBodies: Map[String, Set[SymbolicHeap]], iteration : Int): Boolean = {
    if (partialUnfolding.hasPointer) {
      // Have matched everything in the model, but there is still a pointer left in the unfolding => not a model
      logger.debug("...but unfolding has pointer => no model, aborting branch")
      NoModel
    } else if (partialUnfolding.hasPredCalls) {
      // Try to replace predicate calls with empty heaps if possible; if so, recurse; otherwise return false
      logger.debug("...but unfolding has calls => generating unfoldings with empty spatial part (if any)")
      val unfoldings = unfoldCallsByEmpty(partialUnfolding, headsToBodies)
      logger.debug("Found the following " + unfoldings.size + " empty unfoldings:\n" + unfoldings.mkString("\n"))
      // Depth-first recursion (on arbitrary ordering!) of the models
      lazyRecursiveGreedyUnfolding(formulaToMatch, unfoldings, history, headsToBodies, iteration)
    } else {
      // Return true iff pure constraints of partial unfolding are met
      val res = pureFormulaEntailment(formulaToMatch.pure, partialUnfolding.pure, history)
      logger.debug("Pure entailment check: " + res + " => " + (if (res) " is model" else " no model, abort branch"))
      res
    }
  }

  private def unfoldCallsByEmpty(sh : SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]]) : Set[SymbolicHeap] = {
    if (sh.hasPredCalls) {
      unfoldFirstCallWithSatisfyingBodies(sh, headsToBodies, body => !body.hasPointer && !body.hasPredCalls)
    } else {
      Set(sh)
    }
  }

  private def matchUnfoldedPointer(modelFormula: SymbolicHeap, partialUnfolding: SymbolicHeap, history: History, headsToBodies: Map[String, Set[SymbolicHeap]], iteration : Int): Boolean = {
    val ptrs : Seq[PointsTo] = partialUnfolding.pointers

    val freePtr = try {
      ptrs.find(p => Var.isFV(p.from.getVarOrZero)).get
    } catch {
      case _ : NoSuchElementException => throw new Throwable("The selected model-checking algorithm supports only garbage-free SIDs, but your SID appears to have garbage (Pointers unreachable from free variables)")
    }
    logger.debug("Will match pointer " + freePtr)

    if (areEqualModuloPure(partialUnfolding.pure, freePtr.from.getVarOrZero, NullPtr().getVarOrZero)) {
      logger.debug("Null pointer on the lhs => Unsatisfiable unfolding => abort branch")
      NoModel
    } else {
      // Take equalities into account while looking for a matching pointer
      val matchingPtr = modelFormula.pointers.find(ptr => areEqualModuloPure(modelFormula.pure, freePtr.from.getVarOrZero, ptr.fromAsVar)).get
      logger.debug("Found pointer with matching lhs: " + matchingPtr)

      if (freePtr.to.size != matchingPtr.to.size) {
        // Pointers have different arity => Not model of the formula
        logger.debug("Matching pointers have different number of args => no model => abort branch")
        NoModel
      }
      else {
        // Found a pointer that matches lhs and length of rhs => perform the acutal matching
        applyParamMatchingToHeaps(modelFormula, partialUnfolding, history, headsToBodies, iteration, freePtr, matchingPtr)
      }

    }
  }

  def applyParamMatchingToHeaps(modelFormula: SymbolicHeap, partialUnfolding: SymbolicHeap, history: History, headsToBodies: Map[String, Set[SymbolicHeap]], iteration: Loc, freePtr: PointsTo, matchingPtr: PointsTo): Boolean = {
    // TODO This is also needlessly inefficient; should get rid of the pointer immediately when we find it to avoid second iteration over the seq
    val smallerUnfolding = partialUnfolding.copy(pointers = partialUnfolding.pointers.filterNot(_ == freePtr))
    val smallerModel = modelFormula.copy(pointers = modelFormula.pointers.filterNot(_ == matchingPtr))
    // Add allocation to history for later comparison
    val newHistory = history + matchingPtr.fromAsVar

    // Instantiate existentially quantified variables on both sides as necessary
    logger.debug("Will match parameter lists " + matchingPtr.toAsVarOrZero.mkString(",") + " and " + freePtr.toAsVarOrZero.mkString(","))
    renameToMatchParameters(smallerModel, matchingPtr.toAsVarOrZero, smallerUnfolding, freePtr.toAsVarOrZero, newHistory) match {
      case Some((renamedModel, renamedUnfolding)) =>

        logger.debug("New lhs: " + renamedModel)
        logger.debug("New rhs: " + renamedUnfolding)

        // Check if we now have null pointers on the lhs in the model; if so, abort
        if (renamedModel.pointers.exists(atom => atom.isInstanceOf[PointsTo] && atom.asInstanceOf[PointsTo].from.getVarOrZero == 0)) {
          logger.debug("Introduced null pointer allocation into model " + renamedModel + " => no model => abort branch")
          NoModel
        } else {
          // Recurse for continued matching
          greedyUnfolding(renamedModel, renamedUnfolding, newHistory, headsToBodies, iteration + 1)
        }
      case None =>
        // Matching parameters failed, formula is not a model of the unfolding
        logger.debug("Matching pointers have unmatchcable args => no model => abort branch")
        NoModel
    }
  }

  @tailrec private def renameToMatchParameters(lhs : SymbolicHeap, lhsParams : Seq[Var], rhs : SymbolicHeap, rhsParams : Seq[Var], history: History, maxIntroducedFV : Int = 0) : Option[(SymbolicHeap, SymbolicHeap)] = {
    // We exploit that (a) both sequences have the same length and (b) both heaps have the same number of FVs
    assert(lhsParams.size == rhsParams.size)
    // FIXME We should enforce the same number of FVs, but formula instantiation apparently does not currently deal correctly with numFV
    assert(lhs.freeVars.size <= rhs.freeVars.size)
    //logger.debug("Renaming " + lhs + " / " + rhs + " to match " + lhsParams.mkString(", ") + " / " + rhsParams.mkString(", "))

    def unbindBoundVariable(lvar: Var, rvar: Var): (SymbolicHeap, SymbolicHeap, Int) = {
      val usedFVIdents = Seq(maxIntroducedFV, rhs.numFV) ++ history
      val newFV = Var.mkVar(usedFVIdents.max + 1)
      logger.debug("Introducing new free variable " + PtrVar(newFV) + " replacing " + lvar + " (model formula) and " + rvar + " (unfolding)")
      val newLhs = lhs.instantiateBoundVar(lvar, newFV)
      val newRhs = rhs.instantiateBoundVar(rvar, newFV)
      (newLhs, newRhs, newFV)
    }

    if (lhsParams.isEmpty) {
      Some(lhs,rhs)
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
            if (areEqualModuloPure(lhs.pure, lvar, rvar)) {
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

  private def unfoldAndRecurse(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, history: History, headsToBodies: Map[String, Set[SymbolicHeap]], iteration : Int): Boolean = {
    // Arbitrarily pick first call
    val heaps = unfoldFirstCallWithSatisfyingBodies(partialUnfolding, headsToBodies, _ => true)
    lazyRecursiveGreedyUnfolding(formulaToMatch, heaps, history, headsToBodies, iteration : Int)
  }

  private def unfoldFirstCallWithSatisfyingBodies(sh : SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]], pBody : SymbolicHeap => Boolean) : Set[SymbolicHeap] = {
    val call = sh.predCalls.head
    val applicableBodies = headsToBodies(call.name) filter (pBody)
    logger.debug("Will unfold " + call + " by...\n" + applicableBodies.map("  - " + _).mkString("\n"))
    val unfolded = for (body <- applicableBodies) yield sh.instantiateCall(call, body)
    unfolded
  }

  private def lazyRecursiveGreedyUnfolding(modelFormula : SymbolicHeap, candidateUnfoldings : Set[SymbolicHeap], history: History, headsToBodies: Map[String, Set[SymbolicHeap]], iteration : Int) : Boolean = {
    // Depth-first traversal of candidate unfoldings
    // TODO Better heuristics, e.g. based on #ptrs in base rule and branching factor?
    Combinators.lazyAny(candidateUnfoldings.toSeq.sortWith(_.predCalls.size > _.predCalls.size), (sh : SymbolicHeap) => greedyUnfolding(modelFormula, sh, history, headsToBodies, iteration + 1))
  }

  private def areEqualModuloPure(pure : Seq[PureAtom], fst : Var, snd : Var) : Boolean = {
    val closure : Closure = new ClosureOfAtomSet(pure.toSet)
    closure.getEqualityClass(fst).contains(snd)
  }

  // FIXME Is this the correct entailment check?
  private def pureFormulaEntailment(lhs : Seq[PureAtom], rhs : Seq[PureAtom], history : History) : Boolean = {
    // Add pure formulas according to history
    val allocPure = history map (v => PtrNEq(PtrVar(v), NullPtr()))
    val lhsPure = lhs ++ allocPure
    logger.debug("Checking pure entailment\n     " + lhsPure + "\n |?= " + rhs)
    val lhsClosure : Closure = new ClosureOfAtomSet(lhsPure.toSet)
    val rhsClosure : Closure = new ClosureOfAtomSet(rhs.toSet)

    // TODO Actually there is no need to compute the closure explicitly, should improve this at some point
    // Every equality and disequality in the rhs should be respected by the lhs, apart from explicit equalities of the kind x == x
    def notTautology(atom : PureAtom) = atom.isInstanceOf[PtrNEq] || atom.getVarsWithNull.size == 2
    val lhsClosureSet = lhsClosure.asSetOfAtoms filter notTautology
    val rhsClosureSet = rhsClosure.asSetOfAtoms filter notTautology
    logger.debug(rhsClosureSet + " subset of " + lhsClosureSet + "?")
    rhsClosureSet subsetOf lhsClosureSet
  }

//  private def isVarNullable(sh : SymbolicHeap, fv : Var): Boolean = {
//
//  }

}
