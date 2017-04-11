package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive._

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

  override def isModel(model: Model, formula : SymbolicHeap, sid: SID) = isModel(model, formula, sid, reportProgress = false)

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

    val IterationBound = Integer.MAX_VALUE

    sealed trait Status
    case class EmptyModel() extends Status
    case class ModelAndUnfoldingHavePtr() extends Status
    case class UnfoldingHasOnlyCalls() extends Status
    case class NonEmptyModelAndEmptyUnfolding() extends Status

    private def status(modelFormula : SymbolicHeap, partialUnfolding : SymbolicHeap) : Status = {
      if (!modelFormula.hasPointer) {
        EmptyModel()
      }
      else if (partialUnfolding.hasPointer) {
        ModelAndUnfoldingHavePtr()
      }
      else if (partialUnfolding.hasPredCalls) {
        UnfoldingHasOnlyCalls()
      } else {
        NonEmptyModelAndEmptyUnfolding()
      }
    }

    def run(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, prevHistory: MCHistory): Boolean = {
      val history = prevHistory.nextIteration
      if (history.iteration < IterationBound) {
        logger.debug("#" * 80)
        logger.debug("Iteration " + history.iteration + ": Greedy model checking of \n     " + formulaToMatch + "\n |?= " + partialUnfolding )
        logger.debug("Steps so far:\n" + history.stepLogger)
        if (reportProgress) println("    MC #" + history.iteration + ": " + formulaToMatch + " |?= " + partialUnfolding)

        // The entailment/modelchecking result is trivially false if we have too few FVs on the left. View this as error on the caller's part
//        if (formulaToMatch.numFV < partialUnfolding.numFV) {
//          throw new IllegalStateException("Trivially false MC result: (Intermediate) model " + formulaToMatch + " has " + formulaToMatch.numFV + " free variables, (intermediate) unfolding " + partialUnfolding + " has " + partialUnfolding.numFV)
//        }

        val res = status(formulaToMatch, partialUnfolding) match {
          case EmptyModel() =>
            // No pointers left => Solve the model checking problem for empty models
            logger.debug("Remaining Model " + formulaToMatch + " is now empty")
            checkEmptyModel(formulaToMatch, partialUnfolding, history)

          case ModelAndUnfoldingHavePtr() =>
            // Match pair of pointers that are already there
            logger.debug("Unfolding has pointer, will try to match")
            val unificationResult = PointerUnification.removeUnifiablePointerPair(formulaToMatch, partialUnfolding, disallowedFVs = history.alloc)
            unificationResult match {
              case None => NoModel
              case Some(PointerUnification.PointerMatch(lhsPtr, rhsPtr, newFVs, newFormulaToMatch, newPartialUnfolding)) =>
                val newHistory = history.addAlloc(newFVs).logStep(MCHistory.PointerMatch(formulaToMatch, partialUnfolding, lhsPtr, rhsPtr))
                run(newFormulaToMatch, newPartialUnfolding, newHistory)
            }

          case UnfoldingHasOnlyCalls() =>
            // If none, unfold to (possibly) get pointers
            logger.debug("Unfolding has no pointer but has predicate call, will unfold")
            unfoldFirstCallAndRecurse(formulaToMatch, partialUnfolding, history, considerRulesSatisfying = _ => true)

          case NonEmptyModelAndEmptyUnfolding() =>
            // Unfolding empty, model non-empty => model checking failed
            logger.debug("Model-checking non-empty model against empty candidate unfolding failed, aborting branch")
            NoModel
        }

        logger.debug("Returning from iteration " + history.iteration)
        res
      }
      else false
    }

    /**
      * Checks empty model against arbitrary unfolding
      */
    private def checkEmptyModel(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, history: MCHistory): Boolean = {
      if (partialUnfolding.hasPointer) {
        // Have matched everything in the model, but there is still a pointer left in the unfolding => not a model
        logger.debug("...but unfolding has pointer => no model, aborting branch")
        NoModel
      } else if (partialUnfolding.hasPredCalls) {
        // Try to replace predicate calls with empty heaps if possible; if so, recurse; otherwise return false
        logger.debug("...but unfolding has calls => generating unfoldings with empty spatial part (if any)")
        unfoldFirstCallAndRecurse(formulaToMatch, partialUnfolding, history, considerRulesSatisfying = sh => !sh.hasPointer && !sh.hasPredCalls)
      } else {
        // Return true iff pure constraints of partial unfolding are met
        val res = PureFormulaReasoning.pureFormulaEntailment(formulaToMatch.pure ++ history.toFormula, partialUnfolding.pure)
        logger.debug("Pure entailment check: " + res + " => " + (if (res) " is model" else " no model, abort branch"))
        res
      }
    }

    /**
      * Considers all options of unfolding the first predicate call in the given partial unfolding
      */
    private def unfoldFirstCallAndRecurse(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, history: MCHistory, considerRulesSatisfying : SymbolicHeap => Boolean): Boolean = {
      // Arbitrarily pick first call
      val heaps = SIDUnfolding.unfoldFirstCallWithSatisfyingBodies(headsToBodies, partialUnfolding, considerRulesSatisfying)
      logger.debug("Found the following " + heaps.size + " unfoldings:\n" + heaps.mkString("\n"))
      processHeapsLazilyWith(applyOrderingHeuristics(heaps)) {
        (sh: SymbolicHeap) => run(formulaToMatch, sh, history.logStep(MCHistory.UnfoldingStep(formulaToMatch, partialUnfolding, sh)))
      }
    }

    private def applyOrderingHeuristics(heaps : Iterable[SymbolicHeap]) : Seq[SymbolicHeap] = heaps.toSeq.sortWith(_.predCalls.size > _.predCalls.size)

    @tailrec private def processHeapsLazilyWith(candidates : Seq[SymbolicHeap])(f : SymbolicHeap => Boolean) : Boolean = {
      if (candidates.isEmpty) NoModel else {
        val res = f(candidates.head)
        if (res) res else processHeapsLazilyWith(candidates.tail)(f)
      }
    }

  }

}
