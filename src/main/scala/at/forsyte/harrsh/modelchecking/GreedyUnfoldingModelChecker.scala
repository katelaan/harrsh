package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{ConsistencyCheck, Determinization, PureEntailment}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.IOUtils

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

//  sealed trait MatchingResult {
//    def isModelCheckingSuccess : Boolean = this match {
//      case MatchingSuccess() => true
//      case _ => false
//    }
//
//    def isReducedEntailmentSuccess : Boolean = this match {
//      case MatchingFailure() => false
//      case _ => false
//    }
//  }
//  case class MatchingInconsistent() extends MatchingResult
//  case class MatchingSuccess() extends MatchingResult
//  case class MatchingFailure() extends MatchingResult

  val IsModel = true
  val NoModel = false

  override def isModel(model: Model, formula : SymbolicHeap, sid: SidLike) = isModel(model, formula, sid, reportProgress = false)

  def isModel(model: Model, formula : SymbolicHeap, sid: SidLike, reportProgress: Boolean): Boolean = {

    val modelFormula = ModelToFormula(model)
    val map = headsToBodiesMap(sid)

    new GreedyUnfolding(map, reportProgress).run(modelFormula, formula, MCHistory.emptyHistory)
  }

  def isModel(model : Model, sid : SidLike, reportProgress: Boolean) : Boolean = isModel(model, sid.callToStartPred, sid, reportProgress = reportProgress)

  /**
    * Solve the reduced entailment problem via greedy pointer matching;
    * sound for satisfiable left-hand sides and SIDs without unsatisfiable unfoldings
    * @param lhs Reduced symbolic heap on the lhs of the entailment
    * @param rhs Arbitrary symbolic heap on the rhs of the entailment
    * @param sid Underlying SID
    * @return true iff lhs |= rhs
    */
  def reducedEntailmentAsModelChecking(lhs : SymbolicHeap, rhs : SymbolicHeap, sid : SidLike, reportProgress: Boolean = false): Boolean = {
    assert(lhs.isReduced)
    // Using the model checker for reduced entailment is only sound if the lhs is well-determined
    assert(Determinization.isDetermined(lhs))

    val res = new GreedyUnfolding(headsToBodiesMap(sid), reportProgress).run(lhs, rhs, MCHistory.emptyHistory)
    IOUtils.printIf(reportProgress)("    REDENT result: " + res)
    res
  }

  private def headsToBodiesMap(sid: SidLike): Map[String, Seq[SymbolicHeap]] = sid.preds.map(p => (p.head,p.bodySHs)).toMap

  sealed trait ModelCheckingStatus
  case object EmptyModel extends ModelCheckingStatus
  case object ModelAndUnfoldingHavePtr extends ModelCheckingStatus
  case object UnfoldingHasOnlyCalls extends ModelCheckingStatus
  case object NonEmptyModelAndEmptyUnfolding extends ModelCheckingStatus

  private class GreedyUnfolding(headsToBodies: Map[String, Seq[SymbolicHeap]], reportProgress : Boolean) {

    val IterationBound = Integer.MAX_VALUE

    private def status(modelFormula : SymbolicHeap, partialUnfolding : SymbolicHeap) : ModelCheckingStatus = {
      if (!modelFormula.hasPointer) {
        EmptyModel
      }
      else if (partialUnfolding.hasPointer) {
        ModelAndUnfoldingHavePtr
      }
      else if (partialUnfolding.nonReduced) {
        UnfoldingHasOnlyCalls
      } else {
        NonEmptyModelAndEmptyUnfolding
      }
    }

    def run(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, prevHistory: MCHistory): Boolean = {
      val history = prevHistory.nextIteration
      if (history.iteration < IterationBound) {
        logger.debug("#" * 80)
        logger.debug("Iteration " + history.iteration + ": Greedy model checking of \n     " + formulaToMatch + "\n |?= " + partialUnfolding )
        logger.debug("Steps so far:\n" + history.stepLogger)
        IOUtils.printIf(reportProgress)("    MC #" + history.iteration + ": " + formulaToMatch + " |?= " + partialUnfolding)

        // The entailment/modelchecking result is trivially false if we have too few FVs on the left. View this as error on the caller's part
//        if (formulaToMatch.numFV < partialUnfolding.numFV) {
//          throw new IllegalStateException("Trivially false MC result: (Intermediate) model " + formulaToMatch + " has " + formulaToMatch.numFV + " free variables, (intermediate) unfolding " + partialUnfolding + " has " + partialUnfolding.numFV)
//        }

        val res = status(formulaToMatch, partialUnfolding) match {
          case EmptyModel =>
            // No pointers left => Solve the model checking problem for empty models
            logger.debug("Remaining Model " + formulaToMatch + " is now empty")
            checkEmptyModel(formulaToMatch, partialUnfolding, history)

          case ModelAndUnfoldingHavePtr =>
            // Match pair of pointers that are already there
            logger.debug("Unfolding has pointer, will try to match")
            // TODO It would be better to avoid the conversion, but it would be incorrect to simply keep alloc as set, because we then wouldn't notice double allocation. Maybe optimize this later
            val unificationResult = PointerUnification.removeUnifiablePointerPair(formulaToMatch, partialUnfolding, disallowedFVs = history.alloc.toSet)
            unificationResult match {
              case None => NoModel
              case Some(PointerUnification.PointerMatch(lhsPtr, rhsPtr, newFV, newFormulaToMatch, newPartialUnfolding)) =>
                //val newHistory = history.addAlloc(newFVs).logStep(MCHistory.PointerMatch(formulaToMatch, partialUnfolding, lhsPtr, rhsPtr))
                val newHistory = history.addAlloc(newFV).logStep(MCHistory.PointerMatch(formulaToMatch, partialUnfolding, lhsPtr, rhsPtr))
                run(newFormulaToMatch, newPartialUnfolding, newHistory)
            }

          case UnfoldingHasOnlyCalls =>
            // If none, unfold to (possibly) get pointers
            logger.debug("Unfolding has no pointer but has predicate call, will unfold")
            unfoldFirstCallAndRecurse(formulaToMatch, partialUnfolding, history, considerRulesSatisfying = _ => true)

          case NonEmptyModelAndEmptyUnfolding =>
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
      } else if (partialUnfolding.nonReduced) {
        // Try to replace predicate calls with empty heaps if possible; if so, recurse; otherwise return false
        logger.debug("...but unfolding has calls => generating unfoldings with empty spatial part (if any)")
        unfoldFirstCallAndRecurse(formulaToMatch, partialUnfolding, history, considerRulesSatisfying = sh => !sh.hasPointer && sh.isReduced)
      } else {
        // Return true iff pure constraints of partial unfolding are met
        val historyConstraints = history.toPureConstraints
        val lhsWithImpliedInequalities = formulaToMatch.pure ++ historyConstraints
        IOUtils.printIf(reportProgress)("    Implied LHS constraints " + historyConstraints + " derived from allocation " + history.alloc.mkString(","))
        IOUtils.printIf(reportProgress)("    " + lhsWithImpliedInequalities.mkString("{",", ", "}") + " |?=_PURE " + partialUnfolding.pure.mkString("{",", ", "}"))

        // FIXME Improve efficiency here? It's redundant to check entailment and consistency separately
        val freeVars = Var.freeNonNullVars(lhsWithImpliedInequalities.flatMap(_.getNonNullVars))
        if (!ConsistencyCheck.isConsistent(SymbolicHeap(lhsWithImpliedInequalities, Seq(), Seq(), freeVars))) {
          // Inconsistent history constraints => Not a model
          IOUtils.printIf(reportProgress)("    Inconsistent constraints, not a model")
          false
        } else {
          val res = PureEntailment.check(lhsWithImpliedInequalities, partialUnfolding.pure)
          logger.debug("Pure entailment check: " + res + " => " + (if (res) " is model" else " no model, abort branch"))
          res
        }
      }
    }

    /**
      * Considers all options of unfolding the first predicate call in the given partial unfolding
      */
    private def unfoldFirstCallAndRecurse(formulaToMatch: SymbolicHeap, partialUnfolding: SymbolicHeap, history: MCHistory, considerRulesSatisfying : SymbolicHeap => Boolean): Boolean = {
      // Arbitrarily pick first call
      val heaps = SidUnfolding.unfoldFirstCallWithSatisfyingBodies(headsToBodies, partialUnfolding, considerRulesSatisfying)
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
