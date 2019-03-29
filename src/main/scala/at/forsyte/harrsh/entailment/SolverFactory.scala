package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentChecker.{EntailmentCheckerStats, EntailmentFixedPointStats}
import at.forsyte.harrsh.main.{HarrshLogging, ProblemStatus, SatQuery}
import at.forsyte.harrsh.main.ProblemStatus.{Correct, Incorrect, Unknown}
import at.forsyte.harrsh.pure.{Closure, PureEntailment}
import at.forsyte.harrsh.refinement.{RefinementAlgorithms, SatChecker}
//import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, RichSid}
//import at.forsyte.harrsh.seplog.sidtransformers.GraphInterfaceAnnotator
//import at.forsyte.harrsh.seplog.sidtransformers.GraphInterfaceAutomaton.GraphInterface
import at.forsyte.harrsh.util.IOUtils

object SolverFactory extends HarrshLogging {

  type FixedPoint = Map[String, Set[EntailmentProfile]]

  def apply(config: EntailmentConfig): SolverStrategy[Unit] = {
    SolverStrategy.chain(
      GuardedStrategy(config.performInitialSatCheck, isLhsSatTactic),
      GuardedStrategy(config.patternMatchingLevel > 0, patternMatchingTactic(config.patternMatchingLevel)),
      fixedPointStrategy(config)
    )
  }

  private def fixedPointStrategy(config: EntailmentConfig): SolverStrategy[Unit] = {
    computeFixedPoint(config) andThen topLevelSolver(config)
  }

//  private def constraintByInterface(topLevelConstraint: TopLevelConstraint, interfaces: Map[String, GraphInterface]): Map[GraphInterface, PredCall] = {
//    (topLevelConstraint.calls map (call => (interfaces(call.name).rename(Var.getFvSeq(call.args.length), call.args), call))).toMap
//  }
//
//  private def splitBasedOnInterface(ei: EntailmentInstance): Seq[EntailmentInstance] = {
//    val unionSid = ei.lhs.sid.copy(preds = (ei.lhs.sid.preds ++ ei.rhs.sid.preds).distinct)
//    val interfaces = GraphInterfaceAnnotator(unionSid)
//    logger.warn(s"Computed the following interfaces: $interfaces")
//    val lhsByIf = constraintByInterface(ei.lhs.topLevelConstraint, interfaces)
//    val rhsByIf = constraintByInterface(ei.rhs.topLevelConstraint, interfaces)
//    logger.warn(s"Instantiated LHS interfaces:\n${lhsByIf.mkString("\n")}")
//    logger.warn(s"Instantiated RHS interfaces:\n${rhsByIf.mkString("\n")}")
//    val shared = lhsByIf.keySet intersect rhsByIf.keySet
//    val unmatchedLhs = lhsByIf.filterKeys(!shared(_)).values
//    val unmatchedRhs = rhsByIf.filterKeys(!shared(_)).values
//    val unmatchedLhsConstraint = TopLevelConstraint(unmatchedLhs.toSeq, ei.lhs.topLevelConstraint.pure)
//    val unmatchedRhsConstraint = TopLevelConstraint(unmatchedRhs.toSeq, ei.rhs.topLevelConstraint.pure)
//    val unmatchedInstance = EntailmentInstance(
//      EntailmentQuerySide(ei.lhs.sid, unmatchedLhsConstraint, ei.lhs.originalAssertion),
//      EntailmentQuerySide(ei.rhs.sid, unmatchedRhsConstraint, ei.rhs.originalAssertion),
//      ei.entailmentHolds)
//    val otherInstances: Seq[EntailmentInstance] = shared.toSeq map (gif => toInstance(ei, lhsByIf(gif), rhsByIf(gif)))
//    unmatchedInstance +: otherInstances
//  }

  private def toInstance(originalEi: EntailmentInstance, lhsCall: PredCall, rhsCall: PredCall): EntailmentInstance = {
    EntailmentInstance(
      EntailmentQuerySide(originalEi.lhs.sid, TopLevelConstraint(Seq(lhsCall), Seq.empty), lhsCall.toSymbolicHeap),
      EntailmentQuerySide(originalEi.rhs.sid, TopLevelConstraint(Seq(rhsCall), Seq.empty), rhsCall.toSymbolicHeap),
      originalEi.entailmentHolds
    )
  }

  private def topLevelSolver(config: EntailmentConfig): SolverStrategy[FixedPoint] = {
    makeStats andThen topLevelSolverFrom(if (config.useUnionSolver) UnionSolver else BruteForceSolver)
  }

  private def topLevelSolverFrom(solver: TopLevelSolver): SolverStrategy[FixedPoint] = {
    SolverStrategy.fromFunction { case (ei: EntailmentInstance, fp: FixedPoint) =>
//      val split = splitBasedOnInterface(ei)
//      logger.warn(s"We could split $ei into\n${split.mkString("\n")}")

      val isValid = solver.checkValidity(ei, fp)
      if (isValid) Correct else Incorrect
    }
  }

  private def patternMatchingTactic(level: Int): SolverStrategy[Unit] = {
    SolverStrategy.fromFunction{ (ei, _) =>
      if (patternMatchingCanSucceed(ei)) {
        logger.debug("Trying to solve query by pattern matching...")
        val trivialMatchingResult = solveByImmediatePatternMatching(ei)
        if (trivialMatchingResult == Unknown && level >= 2) solveByDecompMatching(ei, level) else trivialMatchingResult
      } else {
        logger.info("Skipping pattern-matching tactic based on success-prediction heuristic.")
        Unknown
      }
    }
  }

  private def solveByImmediatePatternMatching(ei: EntailmentInstance): ProblemStatus = {
    val res = (ei.lhs.topLevelConstraint.calls.toSet[PredCall] == ei.rhs.topLevelConstraint.calls.toSet[PredCall]) && {
      PureEntailment.check(
        Closure.fromSH(ei.lhs.topLevelConstraint.toSymbolicHeap),
        Closure.fromSH(ei.rhs.topLevelConstraint.toSymbolicHeap))
    }
    if (res) {
      logger.info(s"Entailment holds by pattern matching of ${ei.lhs.topLevelConstraint} |= ${ei.rhs.topLevelConstraint}")
      Correct
    } else {
      Unknown
    }
  }

  private def patternMatchingCanSucceed(ei: EntailmentInstance): Boolean = {
    // TODO: In principle there can be more sophisticated checks here; also, this discards some instances where pattern matching can succeed (non-progress rules whose application results in predicates of the RHS)
    val lhs = ei.lhs.topLevelConstraint
    val lhsLength = lhs.calls.length
    val rhs = ei.rhs.topLevelConstraint
    val rhsLength = rhs.calls.length
    val rhsPreds = ei.rhs.sid.predIdents
    Stream(
      lhsLength >= rhsLength,
      lhsLength == rhsLength || ei.rhs.sid.empClosedNonProgressRules.nonEmpty,
      lhs.calls.forall(rhsPreds contains _.name)) forall (b => b)
  }

  private def solveByDecompMatching(ei: EntailmentInstance, level: Int): ProblemStatus = {
    val lhsAsDecomp = ContextDecomposition.fromTopLevelQuery(ei.lhs.topLevelConstraint, ei.lhs.sid)
    val lhsAfterMerging = if (level >= 3) mergeWithNoProgressRules(lhsAsDecomp, ei.rhs.sid) else Seq(lhsAsDecomp)
    lhsAfterMerging.find(_.isFinal(ei.rhs.sid, ei.rhs.topLevelConstraint)) match {
      case Some(finalDecomp) =>
        logger.info(s"Successful pattern matching of ${ei.lhs.topLevelConstraint} |= ${ei.rhs.topLevelConstraint} => Entailment holds")
        logger.info(s"Decomposition used for pattern matching:\n" + finalDecomp)
        Correct
      case None =>
        Unknown
    }
  }

  private def mergeWithNoProgressRules(decomp: ContextDecomposition, sid: RichSid): Seq[ContextDecomposition] = {
    if (sid.empClosedNonProgressRules.nonEmpty) {
      MergeUsingNonProgressRules.useNonProgressRulesToMergeContexts(decomp, sid)
    } else {
      Seq(decomp)
    }
  }

  private def isLhsSatTactic: SolverStrategy[Unit] = {
    def isLhsSat(ei: EntailmentInstance): ProblemStatus = {
      logger.debug("Will check if LHS is satisfiable. (If not, the entailment holds.)")
      val satQuery = SatQuery(ei.lhs.sid, ei.lhs.topLevelConstraint.toSymbolicHeap, Unknown, None)
      if (SatChecker(satQuery)) {
        Unknown
      } else {
        logger.info(s"$satQuery is unsat => entailment holds")
        Correct
      }
    }
    SolverStrategy.fromFunction(isLhsSat _)
  }

  private lazy val makeStats: StatsGenerator[FixedPoint] = StatsGenerator((_,fp) => EntailmentCheckerStats(entailmentStats(fp)))

  private def computeFixedPoint(config: EntailmentConfig): SolverStateTransformer[Unit, FixedPoint] = {
    SolverStateTransformer.fromFunction(runEntailmentAutomaton(config)_)
  }

  private def runEntailmentAutomaton(config: EntailmentConfig)(entailmentInstance: EntailmentInstance): Map[String, Set[EntailmentProfile]] = {
    val EntailmentInstance(lhs, rhs, _) = entailmentInstance
    val aut = new EntailmentAutomaton(rhs.sid, rhs.topLevelConstraint)
    val (reachableStatesByPred, transitionsByHeadPred) = RefinementAlgorithms.fullRefinementTrace(lhs.sid, aut, config.io.reportProgress)

    if (config.io.printResult) {
      println(FixedPointSerializer(entailmentInstance, markFinalProfiles = false)(reachableStatesByPred))
    }
    if (config.io.exportToLatex) {
      print("Will export result to LaTeX...")
      IOUtils.writeFile("entailment.tex", EntailmentResultToLatex.entailmentFixedPointToLatex(entailmentInstance, aut, reachableStatesByPred, transitionsByHeadPred))
      println(" Done.")
    }
    reachableStatesByPred
  }

  private def entailmentStats(reachableStatesByPred: Map[String, Set[EntailmentProfile]]): EntailmentFixedPointStats = {
    val numExploredPreds = reachableStatesByPred.size
    val allProfiles = reachableStatesByPred.values.flatten.toList
    val allDecomps: Seq[ContextDecomposition] = for {
      c <- allProfiles
      s <- c.decompsOrEmptySet
    } yield s
    val totalNumContexts = allDecomps.map(_.parts.size).sum
    EntailmentFixedPointStats(numExploredPreds, allProfiles.size, allDecomps.size, totalNumContexts)
  }

}
