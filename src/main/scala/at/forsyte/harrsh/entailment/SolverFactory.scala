package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentChecker.{EntailmentCheckerStats, EntailmentFixedPointStats}
import at.forsyte.harrsh.main.{HarrshLogging, ProblemStatus, SatQuery}
import at.forsyte.harrsh.main.ProblemStatus.{Correct, Incorrect, Unknown}
import at.forsyte.harrsh.refinement.{RefinementAlgorithms, SatChecker}
import at.forsyte.harrsh.util.IOUtils

object SolverFactory extends HarrshLogging {

  type FixedPoint = Map[String, Set[EntailmentProfile]]

  def apply(config: EntailmentConfig): SolverStrategy[Unit] = {
    SolverStrategy.chain(
      GuardedStrategy(config.performInitialSatCheck, isLhsSatTactic),
      GuardedStrategy(config.withPatternMatchingStage, patternMatchingTactic),
      fixedPointStrategy(config)
    )
  }

  private def fixedPointStrategy(config: EntailmentConfig): SolverStrategy[Unit] = {
    computeFixedPoint(config) andThen topLevelSolver(config)
  }

  private def topLevelSolver(config: EntailmentConfig): SolverStrategy[FixedPoint] = {
    makeStats andThen topLevelSolverFrom(if (config.useUnionSolver) UnionSolver else BruteForceSolver)
  }

  private def topLevelSolverFrom(solver: TopLevelSolver): SolverStrategy[FixedPoint] = {
    SolverStrategy.fromFunction { case (ei: EntailmentInstance, fp: FixedPoint) =>
      val isValid = solver.checkValidity(ei, fp)
      if (isValid) Correct else Incorrect
    }
  }

  private def patternMatchingTactic: SolverStrategy[Unit] = {
    SolverStrategy.fromFunction{ (ei, _) =>
      logger.warn("Pattern matching tactic not implemented.")
      Unknown
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
      println(FixedPointSerializer(entailmentInstance)(reachableStatesByPred))
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
