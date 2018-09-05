package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID}
import at.forsyte.harrsh.util.IOUtils

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
  def solve(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true): Boolean = {
    val entailmentHolds = runEntailmentAutomaton(entailmentInstance, reportProgress, printResult)

    entailmentInstance.entailmentHolds foreach {
      shouldHold =>
        if (shouldHold != entailmentHolds)
          println(s"Unexpected result: Entailment should hold according to input file: $shouldHold; computed result: $entailmentHolds")
    }

    entailmentHolds
  }

  def runEntailmentAutomaton(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): Boolean = {
    val EntailmentInstance(lhsSid, lhsCall, rhsSid, rhsCall, _) = entailmentInstance
    val aut = new EntailmentAutomaton(rhsSid, rhsCall)
    val (reachableStatesByPred, transitionsByHeadPred) = RefinementAlgorithms.fullRefinementTrace(lhsSid, aut, reportProgress)
    val isFinal = (s: EntailmentAutomaton.State) => aut.isFinal(s)
    val entailmentHolds = reachableStatesByPred(lhsCall.name).forall(isFinal)

    if (printResult) {
      println(serializeResult(aut, reachableStatesByPred))
    }
    if (exportToLatex) {
      print("Will export result to LaTeX...")
      IOUtils.writeFile("entailment.tex", EntailmentInstanceToLatex.entailmentInstanceToLatex(entailmentInstance, entailmentHolds, aut, reachableStatesByPred, transitionsByHeadPred))
      println(" Done.")
    }

    entailmentHolds
  }

  object serializeResult {

    def apply(aut: EntailmentAutomaton, reachable: Map[String, Set[EntailmentAutomaton.State]]): String = {
      val isFinal = (s: EntailmentAutomaton.State) => aut.isFinal(s)
      serializeReach(reachable, isFinal)
    }

    private def indent(s : String) = "  " + s

    def serializeReach(statesByPred: Map[String, Set[EntailmentAutomaton.State]], isFinal: EntailmentAutomaton.State => Boolean): String = {
      val lines = Stream("RESULT {") ++ statesByPred.toStream.flatMap(pair => serializePred(pair._1, pair._2, isFinal)).map(indent) ++ Stream("}")
      lines.mkString("\n")
    }

    def serializePred(pred: String, states: Set[EntailmentAutomaton.State], isFinal: EntailmentAutomaton.State => Boolean): Stream[String] = {
      Stream(s"PRED $pred {") ++ states.toStream.flatMap(s => serializeState(s, isFinal)).map(indent) ++ Stream("}")
    }

    def serializeState(state: EntailmentAutomaton.State, isFinal: EntailmentAutomaton.State => Boolean): Stream[String] = {
      (Stream("STATE {",
        s"  PARAMS: ${state.orderedParams.mkString(", ")}")
        ++ Some("  FINAL").filter(_ => isFinal(state))
        ++ serializeETs(state.ets) ++ Stream("}"))
    }

    def serializeETs(ets: Set[ExtensionType]): Stream[String] = {
      if (ets.nonEmpty) ets.toStream.flatMap(serializeET).map(indent)
      else Stream(indent("NO CONSISTENT ET"))
    }

    def serializeET(et: ExtensionType): Stream[String] = {
      et.toString.lines.toStream
    }

  }

}
