package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.util.CacheRegistry

object EntailmentChecker extends HarrshLogging {

  case class EntailmentStats(numRefinedPreds: Int, numProfiles: Int, totalNumDecomps: Int, totalNumContexts: Int) {
    def prettyPrint: String = {
      s"#Refined predicates:         $numRefinedPreds\n" +
      s"#Computed profiles:          $numProfiles\n" +
      s"#Decompositions in profiles: $totalNumDecomps\n" +
      s"#Contexts in decompositions: $totalNumContexts"
    }
  }

  /**
    * Check whether the entailment solver produces the expected result on the given instance.
    * @param description Description of the instance
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return Computed result + optionally whether the result is as expected?
    */
  def check(description: String, entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean, Option[Boolean], EntailmentStats) = {
    assume(entailmentInstance.usesDefaultFVs)
    val (entailmentHolds,stats) = solve(entailmentInstance, reportProgress, printResult, exportToLatex)
    entailmentInstance.entailmentHolds match {
      case Some(shouldHold) =>
        val expectedResult = shouldHold == entailmentHolds
        if (printResult) println(s"$description: Got expected result: $expectedResult")
        if (!expectedResult) {
          println(s"WARNING: Unexpected result")
        }
        (entailmentHolds, Some(expectedResult), stats)
      case None =>
        if (printResult) println(s"$description: No expected result specified. Computed result: $entailmentHolds")
        (entailmentHolds, None, stats)
    }
  }

  val UseUnionSolver = true
  val Solver: TopLevelSolver = if (UseUnionSolver) UnionSolver else BruteForceSolver

  /**
    * Check whether the given entailment instance holds
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return True iff the entailment holds
    */
  def solve(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean, EntailmentStats) = {
    logger.info(s"Solving $entailmentInstance...")
    CacheRegistry.resetAllCaches()
    val res@(holds, stats) = runSolver(Solver, entailmentInstance, reportProgress, printResult)

    //    if (exportToLatex) {
    //      print("Will export result to LaTeX...")
    //      IOUtils.writeFile("entailment.tex", EntailmentResultToLatex.entailmentCheckingResultToLatex(entailmentInstance, entailmentHolds, aut, reachableStatesByPred, transitionsByHeadPred))
    //      println(" Done.")
    //    }

    entailmentInstance.entailmentHolds foreach {
      shouldHold =>
        if (shouldHold != holds)
          println(s"Unexpected result: Entailment should hold according to input file: $shouldHold; computed result: $holds")
    }

    logger.info("Final cache state: " + CacheRegistry.summary)

    res
  }

  def runSolver(topLevelSolver: TopLevelSolver, entailmentInstance: EntailmentInstance, reportProgress: Boolean, printResult: Boolean): (Boolean, EntailmentStats) = {
    val reachableStatesByPred = runEntailmentAutomaton(entailmentInstance, reportProgress, printResult)
    val entailmentHolds = topLevelSolver.checkValidity(entailmentInstance, reachableStatesByPred)
    val stats = entailmentStats(reachableStatesByPred)
    (entailmentHolds,stats)
  }

  def entailmentStats(reachableStatesByPred: Map[String, Set[EntailmentProfile]]): EntailmentStats = {
    val numExploredPreds = reachableStatesByPred.size
    val allProfiles = reachableStatesByPred.values.flatten.toList
    val allDecomps: Seq[ContextDecomposition] = for {
      c <- allProfiles
      s <- c.decompsOrEmptySet
    } yield s
    val totalNumContexts = allDecomps.map(_.parts.size).sum
    EntailmentStats(numExploredPreds, allProfiles.size, allDecomps.size, totalNumContexts)
  }

  private def runEntailmentAutomaton(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): Map[String, Set[EntailmentProfile]] = {
    val EntailmentInstance(lhs, rhs, _) = entailmentInstance
    val aut = new EntailmentAutomaton(rhs.sid, rhs.topLevelConstraint)
    val (reachableStatesByPred, transitionsByHeadPred) = RefinementAlgorithms.fullRefinementTrace(lhs.sid, aut, reportProgress)

    if (printResult) {
      println(FixedPointSerializer(entailmentInstance)(reachableStatesByPred))
    }
    reachableStatesByPred
  }



}
