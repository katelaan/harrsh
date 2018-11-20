package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.PureEntailment
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.inductive.{PredCall, RuleBody, SID}
import at.forsyte.harrsh.util.IOUtils

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
  def check(description: String, entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean, Option[Boolean], Option[EntailmentStats]) = {
    val (entailmentHolds,maybeStats) = solve(entailmentInstance, reportProgress, printResult, exportToLatex)
    entailmentInstance.entailmentHolds match {
      case Some(shouldHold) =>
        val expectedResult = shouldHold == entailmentHolds
        if (printResult) println(s"$description: Got expected result: $expectedResult")
        if (!expectedResult) {
          println(s"WARNING: Unexpected result")
        }
        (entailmentHolds, Some(expectedResult), maybeStats)
      case None =>
        if (printResult) println(s"$description: No expected result specified. Computed result: $entailmentHolds")
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

  def entailmentStats(reachableStatesByPred: Map[String, Set[EntailmentAutomaton.EntailmentProfile]]): EntailmentStats = {
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
    val isFinal = (s: EntailmentAutomaton.EntailmentProfile) => aut.isFinal(s)
    val entailmentHolds = reachableStatesByPred(lhsCall.name).forall(isFinal)
    val stats = entailmentStats(reachableStatesByPred)

    if (printResult) {
      println(serializeResult(aut, reachableStatesByPred))
    }
    if (exportToLatex) {
      print("Will export result to LaTeX...")
      IOUtils.writeFile("entailment.tex", EntailmentInstanceToLatex.entailmentCheckingResultToLatex(entailmentInstance, entailmentHolds, aut, reachableStatesByPred, transitionsByHeadPred))
      println(" Done.")
    }

    (entailmentHolds,stats)
  }

  object serializeResult {

    def apply(aut: EntailmentAutomaton, reachable: Map[String, Set[EntailmentAutomaton.EntailmentProfile]]): String = {
      val isFinal = (s: EntailmentAutomaton.EntailmentProfile) => aut.isFinal(s)
      serializeReach(reachable, isFinal)
    }

    private def indent(s : String) = "  " + s

    def serializeReach(statesByPred: Map[String, Set[EntailmentAutomaton.EntailmentProfile]], isFinal: EntailmentAutomaton.EntailmentProfile => Boolean): String = {
      val lines = Stream("RESULT {") ++ statesByPred.toStream.flatMap(pair => serializePred(pair._1, pair._2, isFinal)).map(indent) ++ Stream("}")
      lines.mkString("\n")
    }

    def serializePred(pred: String, states: Set[EntailmentAutomaton.EntailmentProfile], isFinal: EntailmentAutomaton.EntailmentProfile => Boolean): Stream[String] = {
      Stream(s"PRED $pred {") ++ states.toStream.flatMap(s => serializeState(s, isFinal)).map(indent) ++ Stream("}")
    }

    def serializeState(state: EntailmentAutomaton.EntailmentProfile, isFinal: EntailmentAutomaton.EntailmentProfile => Boolean): Stream[String] = {
      (Stream("PROFILE {",
        s"  FVS: ${state.orderedParams.mkString(", ")}")
        ++ Some("  ACCEPTING").filter(_ => isFinal(state))
        ++ serializeDecomps(state.profile) ++ Stream("}"))
    }

    def serializeDecomps(decomps: Set[ContextDecomposition]): Stream[String] = {
      if (decomps.nonEmpty) decomps.toStream.flatMap(serializeContextDecomposition).map(indent)
      else Stream(indent("NO CONSISTENT DECOMPOSITION"))
    }

    def serializeContextDecomposition(decomp: ContextDecomposition): Stream[String] = {
      val decompStrs = decomp.parts.toList map (_.toString)
      if (decompStrs.size == 1) Stream(s"Decomp(${decompStrs.head})")
      else {
        val fst = decompStrs.head
        val middle = decompStrs.tail.init
        val last = decompStrs.last
        Stream(s"Decomp($fst),") ++ middle.map("       " + _ + ",") :+ s"       $last)"
      }
    }

  }

}
