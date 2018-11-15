package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.PureEntailment
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.inductive.{PredCall, RuleBody, SID}
import at.forsyte.harrsh.util.IOUtils

object EntailmentChecker extends HarrshLogging {

  case class EntailmentInstance(lhsSid: SID, lhsCall: PredCall, rhsSid: SID, rhsCall: PredCall, entailmentHolds: Option[Boolean])

  case class EntailmentStats(numExploredPreds: Int, numProfiles: Int, totalNumDecomps: Int, totalNumContexts: Int) {
    def prettyPrint: String = {
      s"#Explore predicates:         $numExploredPreds\n" +
      s"#Compute profiles:           $numProfiles\n" +
      s"#Decompositions in profiles: $totalNumDecomps\n" +
      s"#Contexts in decomps.:       $totalNumContexts"
    }
  }

  /**
    * Check whether the entailment solver produces the expected result on the given instance.
    * @param description Description of the instance
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return Computed result + optionally whether the result is as expected?
    */
  def check(description: String, entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean, Option[Boolean], Option[EntailmentStats]) = {
    val (entailmentHolds,maybeStats) = solve(entailmentInstance, reportProgress, printResult, exportToLatex)
    entailmentInstance.entailmentHolds match {
      case Some(shouldHold) =>
        val expectedResult = shouldHold == entailmentHolds
        println(s"$description: Got expected result: $expectedResult")
        if (!expectedResult) {
          println(s"WARNING: Unexpected result")
        }
        (entailmentHolds, Some(expectedResult), maybeStats)
      case None =>
        println(s"$description: No expected result specified. Computed result: $entailmentHolds")
        (entailmentHolds, None, maybeStats)
    }
  }

  def contradictoryAllocationIn(instance: EntailmentInstance): Boolean = {
    (for {
      (sid, call) <- Seq((instance.lhsSid, instance.lhsCall), (instance.rhsSid, instance.rhsCall))
      startPred = sid(call.name)
      rule <- startPred.rules
      body = rule.body
    } yield body.pointers.isEmpty && body.predCalls.isEmpty).forall(b => b)
  }

  sealed trait Allocation
  case object NoAllocation extends Allocation
  case object AllocationInSomeRules extends Allocation
  case object AllocationInAllRules extends Allocation

  private def noAllocationIn(r: RuleBody): Boolean = {
    // If there is a predicate call, there must also be allocation, because we assume progress for all predicates except the top-level one
    // Hence we check if both pointers and rules are empty
    val body = r.body
    body.pointers.isEmpty && body.predCalls.isEmpty
  }

  def allocationInPred(sid: SID, predCall: PredCall): Allocation = {
    val pred = sid(predCall.name)
    val (withoutAlloc, withAlloc) = pred.rules.partition(noAllocationIn)
    if (withAlloc.isEmpty) NoAllocation
    else if (withoutAlloc.isEmpty) AllocationInAllRules
    else AllocationInSomeRules
  }

  def solveViaPureEntailment(entailmentInstance: EntailmentInstance): Boolean = {
    // The entailment holds if for every LHS rule there exists an RHS rule such that the entailment holds between the pair of rules
    val lhsRules = entailmentInstance.lhsSid(entailmentInstance.lhsCall.name).rules
    assert(lhsRules forall noAllocationIn)
    // Discard RHS rules with allocation -- since there is no allocation on the left, they are not relevant for the entailment
    // (If no rule remains after filtering, the entailment is trivially false
    val rhsRules = entailmentInstance.rhsSid(entailmentInstance.rhsCall.name).rules.filter(noAllocationIn)
    lhsRules.forall { rule =>
      val lhsPure = rule.body.pure
      rhsRules.map(_.body.pure).exists(rhsPure => PureEntailment.check(lhsPure, rhsPure))
    }
  }

  /**
    * Check whether the given entailment instance holds
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return True iff the entailment holds
    */
  def solve(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean,Option[EntailmentStats]) = {
    val leftAlloc = allocationInPred(entailmentInstance.lhsSid, entailmentInstance.lhsCall)
    val rightAlloc = allocationInPred(entailmentInstance.rhsSid, entailmentInstance.rhsCall)
    val result = (leftAlloc, rightAlloc) match {
      case (NoAllocation, _) =>
        (solveViaPureEntailment(entailmentInstance), None)
      case (_, NoAllocation) =>
        // Allocation is possible on the left, but not on the right => Entailment can't hold
        (false, None)
      case _ =>
        val (holds, stats) = runEntailmentAutomaton(entailmentInstance, reportProgress, printResult, exportToLatex)
        (holds, Some(stats))
    }

    entailmentInstance.entailmentHolds foreach {
      shouldHold =>
        if (shouldHold != result._1)
          println(s"Unexpected result: Entailment should hold according to input file: $shouldHold; computed result: $result")
    }

    result
  }

  def entailmentStats(reachableStatesByPred: Map[String, Set[EntailmentAutomaton.CutProfile]]): EntailmentStats = {
    val numExploredPreds = reachableStatesByPred.size
    val allProfiles = reachableStatesByPred.values.flatten.toList
    val allDecomps = for {
      c <- allProfiles
      s <- c.profile
    } yield s
    val totalNumContexts = allDecomps.map(_.parts.size).sum
    EntailmentStats(numExploredPreds, allProfiles.size, allDecomps.size, totalNumContexts)
  }

  def runEntailmentAutomaton(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean, EntailmentStats) = {
    val EntailmentInstance(lhsSid, lhsCall, rhsSid, rhsCall, _) = entailmentInstance
    val aut = new EntailmentAutomaton(rhsSid, rhsCall)
    val (reachableStatesByPred, transitionsByHeadPred) = RefinementAlgorithms.fullRefinementTrace(lhsSid, aut, reportProgress)
    val isFinal = (s: EntailmentAutomaton.CutProfile) => aut.isFinal(s)
    val entailmentHolds = reachableStatesByPred(lhsCall.name).forall(isFinal)
    val stats = entailmentStats(reachableStatesByPred)

    if (printResult) {
      println(serializeResult(aut, reachableStatesByPred))
    }
    if (exportToLatex) {
      print("Will export result to LaTeX...")
      IOUtils.writeFile("entailment.tex", EntailmentInstanceToLatex.entailmentInstanceToLatex(entailmentInstance, entailmentHolds, aut, reachableStatesByPred, transitionsByHeadPred))
      println(" Done.")
    }

    (entailmentHolds,stats)
  }

  object serializeResult {

    def apply(aut: EntailmentAutomaton, reachable: Map[String, Set[EntailmentAutomaton.CutProfile]]): String = {
      val isFinal = (s: EntailmentAutomaton.CutProfile) => aut.isFinal(s)
      serializeReach(reachable, isFinal)
    }

    private def indent(s : String) = "  " + s

    def serializeReach(statesByPred: Map[String, Set[EntailmentAutomaton.CutProfile]], isFinal: EntailmentAutomaton.CutProfile => Boolean): String = {
      val lines = Stream("RESULT {") ++ statesByPred.toStream.flatMap(pair => serializePred(pair._1, pair._2, isFinal)).map(indent) ++ Stream("}")
      lines.mkString("\n")
    }

    def serializePred(pred: String, states: Set[EntailmentAutomaton.CutProfile], isFinal: EntailmentAutomaton.CutProfile => Boolean): Stream[String] = {
      Stream(s"PRED $pred {") ++ states.toStream.flatMap(s => serializeState(s, isFinal)).map(indent) ++ Stream("}")
    }

    def serializeState(state: EntailmentAutomaton.CutProfile, isFinal: EntailmentAutomaton.CutProfile => Boolean): Stream[String] = {
      (Stream("STATE {",
        s"  PARAMS: ${state.orderedParams.mkString(", ")}")
        ++ Some("  FINAL").filter(_ => isFinal(state))
        ++ serializeETs(state.profile) ++ Stream("}"))
    }

    def serializeETs(ets: Set[TreeCuts]): Stream[String] = {
      if (ets.nonEmpty) ets.toStream.flatMap(serializeET).map(indent)
      else Stream(indent("NO CONSISTENT ET"))
    }

    def serializeET(et: TreeCuts): Stream[String] = {
      et.toString.lines.toStream
    }

  }

}
