package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.util.{CacheRegistry, IOUtils}

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
  def check(description: String, entailmentInstance: EntailmentInstance, config: EntailmentConfig): (Boolean, Option[Boolean], EntailmentStats) = {
    assume(entailmentInstance.usesDefaultFVs)
    val (entailmentHolds,stats) = solve(entailmentInstance, config)
    entailmentInstance.entailmentHolds match {
      case Some(shouldHold) =>
        val expectedResult = shouldHold == entailmentHolds
        if (config.io.printResult) println(s"$description: Got expected result: $expectedResult")
        if (!expectedResult) {
          println(s"WARNING: Unexpected result")
        }
        (entailmentHolds, Some(expectedResult), stats)
      case None =>
        if (config.io.printResult) println(s"$description: No expected result specified. Computed result: $entailmentHolds")
        (entailmentHolds, None, stats)
    }
  }

  /**
    * Check whether the given entailment instance holds
    * @param entailmentInstance Instance to solve
    * @param config Configuration
    * @return True iff the entailment holds
    */
  def solve(entailmentInstance: EntailmentInstance, config: EntailmentConfig): (Boolean, EntailmentStats) = {
    logger.info(s"Solving $entailmentInstance...")
    CacheRegistry.resetAllCaches()
    val solver: TopLevelSolver = if (config.useUnionSolver) UnionSolver else BruteForceSolver
    val res@(holds, stats) = runSolver(solver, entailmentInstance, config)
    entailmentInstance.entailmentHolds foreach {
      shouldHold =>
        if (shouldHold != holds)
          println(s"Unexpected result: Entailment should hold according to input file: $shouldHold; computed result: $holds")
    }

    logger.info("Final cache state: " + CacheRegistry.summary)

    res
  }

  def runSolver(topLevelSolver: TopLevelSolver, entailmentInstance: EntailmentInstance, config: EntailmentConfig): (Boolean, EntailmentStats) = {
    val reachableStatesByPred = runEntailmentAutomaton(entailmentInstance, config)
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

  private def runEntailmentAutomaton(entailmentInstance: EntailmentInstance, config: EntailmentConfig): Map[String, Set[EntailmentProfile]] = {
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



}
