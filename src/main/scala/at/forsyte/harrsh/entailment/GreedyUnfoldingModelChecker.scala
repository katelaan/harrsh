package at.forsyte.harrsh.entailment
import at.forsyte.harrsh.heapautomata.utils.{Closure, ClosureOfAtomSet}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{MapBasedRenaming, PtrVar, Var}
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

  val IsModel = true
  val NoModel = false

  override def isModel(model: Model, formula : SymbolicHeap, sid: SID): Boolean = {

    val modelFormula = ModelToFormula(model)
    val map = SID.rulesToHeadBodyMap(sid)

    greedyUnfolding(modelFormula, formula, map)
  }

  private def greedyUnfolding(formulaToMatch : SymbolicHeap, partialUnfolding : SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]]) : Boolean = {
    logger.debug("Greedy model checking of {\n    " + formulaToMatch + "} against {\n    " + partialUnfolding +"\n}")
    if (formulaToMatch.numFV > partialUnfolding.numFV) {
      // FIXME We should enforce the same number of FVs, but formula instantiation apparently does not currently deal correctly with numFV
      throw new IllegalStateException("Greedy model checker can only deal with tight models, but (intermediate) model has " + formulaToMatch.numFV + " free variables, (intermediate) unfolding has " + partialUnfolding.numFV)
    }

    if (!formulaToMatch.hasPointer) {
      // No pointers left => Solve the model checking problem for empty models
      checkEmptyModel(formulaToMatch, partialUnfolding, headsToBodies)
    } else {
      if (partialUnfolding.hasPointer) {
        // Match pair of pointers that are already there
        matchUnfoldedPointer(formulaToMatch, partialUnfolding, headsToBodies)
      } else if (partialUnfolding.hasPredCalls) {
        // If none, unfold to (possibly) get pointers
        unfoldAndRecurse(formulaToMatch, partialUnfolding, headsToBodies)
      } else {
        // Unfolding empty, model non-empty => model checking failed
        logger.debug("Model-checking non-empty model against empty candidate unfolding failed.")
        NoModel
      }
    }
  }

  private def checkEmptyModel(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]]): Boolean = {
    logger.debug("Remaining Model " + formulaToMatch + " is now empty...")

    if (partialUnfolding.hasPointer) {
      // Have matched everything in the model, but there is still a pointer left in the unfolding => not a model
      logger.debug("...but unfolding has pointer => no model")
      NoModel
    } else if (partialUnfolding.hasPredCalls) {
      // Try to replace predicate calls with empty heaps if possible; if so, recurse; otherwise return false
      logger.debug("...but unfolding has calls => generating unfoldings with empty spatial part (if any)")
      val unfoldings = unfoldCallsByEmpty(partialUnfolding, headsToBodies)
      logger.debug("Found the following " + unfoldings.size + " empty unfoldings:\n" + unfoldings.mkString("\n"))
      // Depth-first recursion (on arbitrary ordering!) of the models
      lazyRecursiveGreedyUnfolding(formulaToMatch, unfoldings, headsToBodies)
    } else {
      // Return true iff pure constraints of partial unfolding are met
      pureFormulaEntailment(formulaToMatch.pure, partialUnfolding.pure)
    }
  }

  private def unfoldCallsByEmpty(sh : SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]]) : Set[SymbolicHeap] = {
    if (sh.hasPredCalls) {
      unfoldFirstCallWithSatisfyingBodies(sh, headsToBodies, body => !body.hasPointer && !body.hasPredCalls)
    } else {
      Set(sh)
    }
  }

  // FIXME Implement pure formula entailment
  private def pureFormulaEntailment(lhs : Seq[PureAtom], rhs : Seq[PureAtom]) : Boolean = IsModel

  private def matchUnfoldedPointer(modelFormula: SymbolicHeap, partialUnfolding: SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]]): Boolean = {
    val ptrs : Seq[PointsTo] = partialUnfolding.pointers

    val freePtr = try {
      ptrs.find(p => Var.isFV(p.from.getVarOrZero)).get
    } catch {
      case _ : NoSuchElementException => throw new Throwable("The selected model-checking algorithm supports only garbage-free SIDs, but your SID appears to have garbage (Pointers unreachable from free variables)")
    }
    logger.debug("Will match pointer " + freePtr)

    // FIXME Can we always find a matching pointer without following pointer equalities? Probably not...
    // FIXME The linear structure of the model pointers is super inefficient, should use a map representation (indexed by variable on the left) instead
    val matchingPtr = modelFormula.pointers.find(_.from == freePtr.from).get
    logger.debug("Matching pointer " + matchingPtr)

    if (freePtr.to.size != matchingPtr.to.size) {
      // Pointers have different arity => Not model of the formula
      NoModel
    }
    else {
      // TODO This is also needlessly inefficient; should get rid of the pointer immediately when we find it to avoid second iteration over the seq
      val smallerUnfolding = partialUnfolding.copy(spatial = modelFormula.spatial.filterNot(_ == freePtr))
      val smallerModel = modelFormula.copy(spatial = modelFormula.spatial.filterNot(_ == matchingPtr))

      // Instantiate existentially quantified variables on both sides as necessary
      logger.debug("Matching parameter lists " + matchingPtr.toAsVar.mkString(",") + " and " + freePtr.toAsVar.mkString(","))
      renameToMatchParameters(smallerModel, matchingPtr.toAsVar, smallerUnfolding, freePtr.toAsVar) match {
        case Some((renamedModel, renamedUnfolding)) =>
          // Recurse for continued matching
          greedyUnfolding(renamedModel, renamedUnfolding, headsToBodies)
        case None =>
          // Matching parameters failed, formula is not a model of the unfolding
          NoModel
      }
    }
  }

  @tailrec private def renameToMatchParameters(lhs : SymbolicHeap, lhsParams : Seq[Var], rhs : SymbolicHeap, rhsParams : Seq[Var]) : Option[(SymbolicHeap, SymbolicHeap)] = {
    // We exploit that (a) both sequences have the same length and (b) both heaps have the same number of FVs
    assert(lhsParams.size == rhsParams.size)
    // FIXME We should enforce the same number of FVs, but formula instantiation apparently does not currently deal correctly with numFV
    assert(lhs.freeVars.size <= rhs.freeVars.size)

    // FIXME Do we need special null treatment?
    if (lhsParams.isEmpty) {
      Some(lhs,rhs)
    } else {
      val (lvar, rvar) = (lhsParams.head, rhsParams.head)
      if (lvar == rvar) {
        renameToMatchParameters(lhs, lhsParams.tail, rhs, rhsParams.tail)
      } else {
        (Var.isFV(lvar), Var.isFV(rvar)) match {
          case (true, true) =>
            // The vars are different FVs => Need to be equal under pure in the model...
            if (areEqualModuloPure(lhs.pure, lvar, rvar)) {
              // ...in which case we don't need to do any renaming...
              renameToMatchParameters(lhs, lhsParams.tail, rhs, rhsParams.tail)
            } else {
              // ...but if they are not the same in the candidate model, it's not a model of rhs
              None
            }
          case (true, false) =>
            // Left is free, right is quantified => Instantiate right-hand side
            logger.debug("Renaming " + rvar + " to " + lvar + " in unfolding")
            val newRhs = rhs.instantiateBoundVar(rvar, lvar)
            renameToMatchParameters(lhs, lhsParams.tail, newRhs, rhsParams.tail)
          case (false, true) =>
            // Left is quantified, right is free => Instantiate left-hand side
            logger.debug("Renaming " + lvar + " to " + rvar + " in model formula")
            val newLhs = lhs.instantiateBoundVar(lvar, rvar)
            renameToMatchParameters(newLhs, lhsParams.tail, rhs, rhsParams.tail)
          case (false, false) =>
            // Both are bound; need to introduce new free var to continue matching
            val newFV = Var.mkVar(lhs.numFV + 1)
            logger.debug("Introducing new free variable " + PtrVar(newFV) + "replacing " + lvar + " (model formula) and " + rvar + " (unfolding)")
            val newLhs = lhs.instantiateBoundVar(lvar, newFV)
            val newRhs = rhs.instantiateBoundVar(rvar, newFV)
            renameToMatchParameters(newLhs, lhsParams.tail, newRhs, rhsParams.tail)
        }
      }
    }
  }

  private def unfoldAndRecurse(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]]): Boolean = {
    // Arbitrarily pick first call
    // TODO Implement unfolding heuristics to stay as close to linear time as possible. (See also comment in lazyRecursiveGreedyUnfolding)
    val heaps = unfoldFirstCallWithSatisfyingBodies(partialUnfolding, headsToBodies, _ => true)
    lazyRecursiveGreedyUnfolding(formulaToMatch, heaps, headsToBodies)
  }

  private def unfoldFirstCallWithSatisfyingBodies(sh : SymbolicHeap, headsToBodies: Map[String, Set[SymbolicHeap]], pBody : SymbolicHeap => Boolean) : Set[SymbolicHeap] = {
    val call = sh.predCalls.head
    val emptyBodies = headsToBodies(call.name) filter (pBody)
    val unfolded = for (body <- emptyBodies) yield sh.instantiateCall(call, body)
    unfolded flatMap (unfoldCallsByEmpty(_, headsToBodies))
  }

  private def lazyRecursiveGreedyUnfolding(formulaToMatch : SymbolicHeap, candidateUnfoldings : Set[SymbolicHeap], headsToBodies: Map[String, Set[SymbolicHeap]]) : Boolean = {
    // Depth-first traversal of candidate unfoldings
    Combinators.lazyAny(candidateUnfoldings.toSeq, (sh : SymbolicHeap) => greedyUnfolding(formulaToMatch, sh, headsToBodies))
    // TODO Depending on branching of recursive rules, change sorting!
    // Explanation: We could sort according to descending size of the spatial part to avoid unfolding with empty bodies. In the case of linear structures, this will be a big benefit (only the very last unfolding will be empty). In cases with at least two predicate calls in recursive calls, this is, however, not true, as in that case a large portion of unfoldings will actually have to use the base rule.
    //Combinators.lazyAny(candidateUnfoldings.toSeq.sortWith(_.spatial.size > _.spatial.size), greedyUnfolding(formulaToMatch, _, headsToBodies))
  }

  private def areEqualModuloPure(pure : Seq[PureAtom], fst : Var, snd : Var) : Boolean = {
    val closure : Closure = new ClosureOfAtomSet(pure.toSet)
    closure.getEqualityClass(fst).contains(snd)
  }

}
