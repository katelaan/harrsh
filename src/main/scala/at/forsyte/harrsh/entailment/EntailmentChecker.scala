package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.refinement.RefinementAlgorithms
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RichSid}
import at.forsyte.harrsh.util.{Combinators, IOUtils}

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

  /**
    * Check whether the given entailment instance holds
    * @param entailmentInstance Instance to solve
    * @param reportProgress Produce additional output to keep track of progress
    * @return True iff the entailment holds
    */
  def solve(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean, EntailmentStats) = {
    logger.debug(s"Solving $entailmentInstance...")
    val res@(holds, stats) = runEntailmentAutomaton(entailmentInstance, reportProgress, printResult, exportToLatex)

    entailmentInstance.entailmentHolds foreach {
      shouldHold =>
        if (shouldHold != holds)
          println(s"Unexpected result: Entailment should hold according to input file: $shouldHold; computed result: $holds")
    }

    res
  }

  def entailmentStats(reachableStatesByPred: Map[String, Set[EntailmentProfile]]): EntailmentStats = {
    val numExploredPreds = reachableStatesByPred.size
    val allProfiles = reachableStatesByPred.values.flatten.toList
    val allDecomps: Seq[ContextDecomposition] = for {
      c <- allProfiles
      s <- c.decomps
    } yield s
    val totalNumContexts = allDecomps.map(_.parts.size).sum
    EntailmentStats(numExploredPreds, allProfiles.size, allDecomps.size, totalNumContexts)
  }

  def runEntailmentAutomaton(entailmentInstance: EntailmentInstance, reportProgress: Boolean = true, printResult: Boolean = true, exportToLatex: Boolean = true): (Boolean, EntailmentStats) = {
    val EntailmentInstance(lhs, rhs, _) = entailmentInstance
    val aut = new EntailmentAutomaton(rhs.sid, rhs.calls)
    val (reachableStatesByPred, transitionsByHeadPred) = RefinementAlgorithms.fullRefinementTrace(lhs.sid, aut, reportProgress)

    if (printResult) {
      println(serializeFixedPoint(aut, reachableStatesByPred))
    }

    val entailmentHolds = checkAcceptance(entailmentInstance.rhs.sid, entailmentInstance.lhs.calls, entailmentInstance.rhs.calls, reachableStatesByPred)
//    if (exportToLatex) {
//      print("Will export result to LaTeX...")
//      IOUtils.writeFile("entailment.tex", EntailmentResultToLatex.entailmentCheckingResultToLatex(entailmentInstance, entailmentHolds, aut, reachableStatesByPred, transitionsByHeadPred))
//      println(" Done.")
//    }

    val stats = entailmentStats(reachableStatesByPred)
    (entailmentHolds,stats)
  }

  private def checkAcceptance(sid: RichSid, lhsConstraint: TopLevelConstraint, rhsConstraint: TopLevelConstraint, reachable: Map[String, Set[EntailmentProfile]]): Boolean = {
    logger.debug(s"Will check whether all profiles in fixed point for $lhsConstraint imply $rhsConstraint wrt $sid")
    if (!rhsConstraint.isQuantifierFree) throw new IllegalArgumentException("RHS is quantified")
    val renamedReachableStates = for {
      call <- lhsConstraint.calls
      // Note: Because of ALL-SAT of the underlying SID, emptiness of the fixed point means that there is no way to
      // express the predicate wrt the RHS-SID as opposed to that the predicate is UNSAT. For this reason, we add
      // an empty profile in case the fixed point does not contain an entry for the predicate.
      reachableForCall = reachable.getOrElse(call.name, Set(EntailmentProfile(Set.empty, Var.getFvSeq(call.args.length))))
      _ = if (reachableForCall.isEmpty) throw new Exception(""+call)
      // We do not make the ALL-SAT assumption for the top-level formula. Instantiating profiles with the call args
      // can thus yield an inconsistent profile. We discard such profiles here
    } yield reachableForCall flatMap (_.renameOrFail(sid, call.args))
    // If the LHS does not contain *any* calls, we must always compute a pure profile:
    // Otherwise, we would get the empty profile for the LHS emp and would thus erroneously conclude that
    // there is no consistent profile for emp
    val profileForLhsPureConstraints = pureProfile(lhsConstraint.pure, computeEvenIfEmpty = lhsConstraint.calls.isEmpty)
    val combinedProfiles = for {
      toplevelStatesForCalls <- Combinators.choices(renamedReachableStates.map(_.toSeq))
      toplevelStates = profileForLhsPureConstraints match {
        case None => toplevelStatesForCalls
        case Some(pureProfile) => pureProfile +: toplevelStatesForCalls
      }
      // TODO: Is it correct that we don't want/need all composition steps of TargetProfile.composeAndForget? (Since emp closure is done in the acceptance check, and focus check is thus not necessary)
      composed <- EntailmentProfileComposition.composeAll(sid, toplevelStates, lhsConstraint.nonNullVars)
      merged = EntailmentProfileComposition.mergeUsingNonProgressRules(composed, sid)
      restricted = if (lhsConstraint.isQuantifierFree) merged else merged.forget(lhsConstraint.boundVars)
    } yield restricted

    logger.debug(combinedProfiles.size + " combined profile(s):\n" + combinedProfiles.mkString("\n"))

    if (combinedProfiles.isEmpty) {
      logger.info(s"There is no profile for $lhsConstraint => $lhsConstraint is unsatisfiable => entailment holds.")
      true
    } else {
      combinedProfiles.forall { p =>
        logger.debug(s"Will check if $p is final...")
        p.decomps.exists(decomp => decomp.isFinal(sid, rhsConstraint))
      }
    }
  }

  private def pureProfile(atoms: Seq[PureAtom], computeEvenIfEmpty: Boolean): Option[EntailmentProfile] = {
    if (!computeEvenIfEmpty && atoms.isEmpty) None
    else {
      val vars = atoms.flatMap(_.getVars).distinct
      val constraints = VarConstraints.fromAtoms(vars.toSet, atoms)
      val decomp = ContextDecomposition(Set.empty, constraints)
      val profile = EntailmentProfile(Set(decomp), vars.filter(!_.isNull))
      logger.debug(s"Created pure profile $profile from top-level atoms $atoms")
      Some(profile)
    }
  }

  object serializeFixedPoint {

    def apply(aut: EntailmentAutomaton, reachable: Map[String, Set[EntailmentProfile]]): String = {
      val isFinal = (s: EntailmentProfile) => aut.isFinal(s)
      serializeReach(reachable, isFinal)
    }

    private def indent(s : String) = "  " + s

    def serializeReach(statesByPred: Map[String, Set[EntailmentProfile]], isFinal: EntailmentProfile => Boolean): String = {
      val lines = Stream("FIXED_POINT {") ++ statesByPred.toStream.flatMap(pair => serializePred(pair._1, pair._2, isFinal)).map(indent) ++ Stream("}")
      lines.mkString("\n")
    }

    def serializePred(pred: String, states: Set[EntailmentProfile], isFinal: EntailmentProfile => Boolean): Stream[String] = {
      Stream(s"PRED $pred {") ++ states.toStream.flatMap(s => serializeState(s, isFinal)).map(indent) ++ Stream("}")
    }

    def serializeState(state: EntailmentProfile, isFinal: EntailmentProfile => Boolean): Stream[String] = {
      (Stream("PROFILE {",
        s"  FVS: ${state.orderedParams.mkString(", ")}")
        ++ Some("  ACCEPTING").filter(_ => isFinal(state))
        ++ serializeDecomps(state.decomps) ++ Stream("}"))
    }

    def serializeDecomps(decomps: Set[ContextDecomposition]): Stream[String] = {
      if (decomps.nonEmpty) decomps.toStream.flatMap(serializeContextDecomposition).map(indent)
      else Stream(indent("NO CONSISTENT DECOMPOSITION"))
    }

    def serializeContextDecomposition(decomp: ContextDecomposition): Stream[String] = {
      val decompStrs = decomp.parts.toList map (_.toString)
      val indent = "       "
      val fst = if (decompStrs.nonEmpty) decompStrs.head else "empty"
      val tail = if (decompStrs.nonEmpty) decompStrs.tail else Seq.empty
      val constraintsStr = indent + decomp.constraints
      Stream(s"Decomp($fst,") ++ tail.map(indent + _ + ",") ++ Stream(constraintsStr + ")")

    }

  }

}
