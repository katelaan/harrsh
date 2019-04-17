package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.{HarrshLogging, ProblemStatus}
import at.forsyte.harrsh.util.CacheRegistry

object EntailmentChecker extends HarrshLogging {

  case class EntailmentFixedPointStats(numRefinedPreds: Int, numProfiles: Int, totalNumDecomps: Int, totalNumContexts: Int) {
    def prettyPrint: String = {
      s"#Refined predicates:         $numRefinedPreds\n" +
      s"#Computed profiles:          $numProfiles\n" +
      s"#Decompositions in profiles: $totalNumDecomps\n" +
      s"#Contexts in decompositions: $totalNumContexts"
    }
  }

  case class EntailmentCheckerStats(stats: EntailmentFixedPointStats) extends AnyVal

  case class EntailmentCheckerResult(status: ProblemStatus, maybeStats: Option[EntailmentCheckerStats])

  /**
    * Check whether the entailment solver produces the expected result on the given instance.
    * @param description Description of the instance
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return Computed result + optionally whether the result is as expected?
    */
  def check(description: String, entailmentInstance: EntailmentInstance, config: EntailmentConfig): (ProblemStatus, Option[Boolean], Option[EntailmentFixedPointStats]) = {
    assume(entailmentInstance.usesDefaultFVs)
    val res = solve(entailmentInstance, config)
    val isAsExpected = checkExpectation(description, entailmentInstance, config, res)
    (res.status, isAsExpected, res.maybeStats.map(_.stats))
  }

  def checkExpectation(description: String, entailmentInstance: EntailmentInstance, config: EntailmentConfig, res: EntailmentCheckerResult): Option[Boolean] = {
    (entailmentInstance.entailmentHolds, res.status.toBoolean) match {
      case (Some(shouldHold), Some(doesHold)) =>
        val gotExpectedResult = shouldHold == doesHold
        if (config.io.printResult) println(s"$description: Got expected result: $gotExpectedResult")
        if (!gotExpectedResult) {
          println(s"WARNING: Unexpected result")
        }
        Some(gotExpectedResult)
      case (Some(_), None) =>
        println("Entailment checker did not compute result.")
        None
      case (None, Some(_)) =>
        println("No expected value specified")
        None
      case (None, None) =>
        None
    }
  }

  /**
    * Check whether the given entailment instance holds
    * @param entailmentInstance Instance to solve
    * @param config Configuration
    * @return True iff the entailment holds
    */
  def solve(entailmentInstance: EntailmentInstance, config: EntailmentConfig): EntailmentCheckerResult = {
    logger.info(s"Solving $entailmentInstance...")
    CacheRegistry.resetAllCaches()
    val solver = SolverFactory(config)
    val res = solver(entailmentInstance, ())
    warnIfUnexpected(entailmentInstance, res)
    logger.info("Final cache state: " + CacheRegistry.summary)
    res
  }

  private def warnIfUnexpected(ei: EntailmentInstance, result: EntailmentCheckerResult): Unit = {
    if (ei.entailmentHolds.isDefined && ei.entailmentHolds != result.status.toBoolean) {
      println(s"Unexpected result: Entailment should hold according to input file: ${ei.entailmentHolds.get}; computed result: ${result.status}")
    }
  }

}
