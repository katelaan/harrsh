package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID}

object EntailmentChecker {

  case class EntailmentInstance(lhsSid: SID, lhsCall: PredCall, rhsSid: SID, rhsCall: PredCall, entailmentHolds: Option[Boolean])

  /**
    * Check whether the entailment solver produces the expected result on the given instance.
    * @param description Description of the instance
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return Is the result as expected?
    */
  def check(description: String, entailmentInstance: EntailmentInstance, reportProgress: Boolean = true): Boolean = {
    val entailmentHolds = runEntailmentAutomaton(entailmentInstance, reportProgress)
    entailmentInstance.entailmentHolds match {
      case Some(shouldHold) =>
        val expectedResult = shouldHold == entailmentHolds
        println(s"$description: Got expected result: $expectedResult")
        if (!expectedResult) {
          println(s"WARNING: Unexpected result")
        }
        expectedResult
      case None =>
        println(s"$description: No expected result. Computed result: $entailmentHolds")
        true
    }
  }

  /**
    * Check whether the given entailment instance holds
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return True iff the entailment holds
    */
  def solve(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true): Boolean = {
    val entailmentHolds = runEntailmentAutomaton(entailmentInstance, reportProgress)

    entailmentInstance.entailmentHolds foreach {
      shouldHold =>
        if (shouldHold != entailmentHolds)
          println(s"Unexpected result: Entailment should hold according to input file: $shouldHold; computed result: $entailmentHolds")
    }

    entailmentHolds
  }

  def runEntailmentAutomaton(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true): Boolean = {
    val EntailmentInstance(lhsSid, lhsCall, rhsSid, rhsCall, _) = entailmentInstance
    val aut = new EntailmentAutomaton(rhsSid, rhsCall)
    val reachable: Set[(String, EntailmentAutomaton.State)] = RefinementAlgorithms.allReachableStates(lhsSid, aut, reportProgress)
    val isFinal = (s: EntailmentAutomaton.State) => aut.isFinal(s)
    reachable.forall{
      case (pred, state) => pred != lhsCall.name || isFinal(state)
    }
  }

}
