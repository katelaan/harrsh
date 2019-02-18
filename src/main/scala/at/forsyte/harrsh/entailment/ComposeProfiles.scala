package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, Predicate, RuleBody, SID}
import at.forsyte.harrsh.util.Combinators

object ComposeProfiles extends HarrshLogging {

  // TODO: Get rid of the second parameter once we use sets. (The new parameters then simply are the union of the params of the constituting profiles)
  object composeAll {

    def apply(profiles: Seq[EntailmentProfile], newOrderedParams: Seq[Var]): EntailmentProfile = {
      // TODO: Exploit associativity of composition when composing more than two profiles
      val composedDecomps = allPossibleDecompCompositions(profiles map (_.decomps))
      val res = EntailmentProfile(composedDecomps, newOrderedParams)
      logger.debug(s"Profile composition result:\n${if (res.nonEmpty) res.decomps.mkString("\n") else "empty (sink state)"}")
      res
    }

    private def allPossibleDecompCompositions(decompsBySource: Seq[Set[ContextDecomposition]]): Set[ContextDecomposition] = {
      for {
        decomps <- Combinators.choices(decompsBySource map (_.toSeq)).toSet[Seq[ContextDecomposition]]
        combined <- composeAllDecomps(decomps)
      } yield combined
    }

    private def composeAllDecomps(decomps: Seq[ContextDecomposition]): Seq[ContextDecomposition] = {
      if (decomps.size <= 1) decomps else {
        for {
          combinedHead <- decomps.head compositionOptions decomps.tail.head
          allComposed <- composeAllDecomps(combinedHead +: decomps.drop(2))
        } yield allComposed
      }
    }

  }

  object mergeUsingNonProgressRules {
    // TODO: Simplify application of non-progress rules to profiles?

    def apply(profile: EntailmentProfile, sid: SID): EntailmentProfile = {
      val newDecomps = for {
        decomp <- profile.decomps
        merged <- useNonProgressRulesToMergeContexts(decomp, sid)
      } yield merged
      EntailmentProfile(newDecomps, profile.orderedParams)
    }

    def useNonProgressRulesToMergeContexts(decomp: ContextDecomposition, sid: SID): Seq[ContextDecomposition] = {
      val nonProgressRules = sid.rulesWithoutPointers
      if (nonProgressRules.nonEmpty && !decomp.isSingletonDecomposition) {
        logger.debug(s"Will try to apply non-progress rules to contexts in decomposition. Rules to consider: ${nonProgressRules.map(pair => s"${pair._1.defaultCall} <= ${pair._2}")}")
        mergeWithZeroOrMoreRuleApplications(decomp, nonProgressRules, sid)
      }
      else {
        val msg = if (decomp.isSingletonDecomposition) "Singleton decomposition => No merging via non-progress rules possible"
        else "The SID does not contain any non-progress rule. Will return decomposition as is."
        logger.debug(msg)
        Seq(decomp)
      }
    }

    private def mergeWithZeroOrMoreRuleApplications(decomp: ContextDecomposition, rules: Seq[(Predicate, RuleBody)], sid: SID): Seq[ContextDecomposition] = {
      val afterMerging = for {
        decompAfterRuleApplication <- applyAllPossibleRulesInMerge(decomp, rules)
        afterAdditionalApplications <- mergeWithZeroOrMoreRuleApplications(decompAfterRuleApplication, rules, sid)
      } yield afterAdditionalApplications

      if (afterMerging.isEmpty) {
        Seq(decomp)
      } else {
        afterMerging
      }
    }

    private def applyAllPossibleRulesInMerge(decomp: ContextDecomposition, rules: Seq[(Predicate, RuleBody)]): Stream[ContextDecomposition] = {
      rules.toStream.flatMap(rule => applyRuleInMerge(decomp, rule._2, rule._1))
    }

    private def applyRuleInMerge(decomp: ContextDecomposition, rule: RuleBody, pred: Predicate): Option[ContextDecomposition] = {
      assert(!rule.hasPointer)
      val callsInRule = rule.body.predCalls
      val roots = decomp.parts map (_.root)
      if (callsInRule.size > roots.size)
      // The rule contains more calls than the ETypes has parts => Rule can't be applicable
        None
      else {
        // FIXME: More efficient choice of possible pairings for matching (don't brute force over all seqs)
        val possibleMatchings = Combinators.allSeqsWithoutRepetitionOfLength(callsInRule.length, roots)
        possibleMatchings.toStream.flatMap(tryMergeGivenRoots(decomp, rule, pred, _)).headOption
      }
    }

    private def tryMergeGivenRoots(decomp: ContextDecomposition, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[ContextPredCall]): Option[ContextDecomposition] = {
      val callsInRule = rule.body.predCalls
      assert(rootsToMerge.size == callsInRule.size)
      if (predicatesMatch(callsInRule, rootsToMerge)) {
        tryArgumentMatching(decomp, rule, pred, rootsToMerge)
      }
      else {
        None
      }
    }

    private def predicatesMatch(calls: Seq[PredCall], labels: Seq[ContextPredCall]): Boolean = {
      (calls, labels).zipped forall {
        case (call, label) => call.name == label.pred.head
      }
    }

    private def tryArgumentMatching(decomp: ContextDecomposition, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[ContextPredCall]): Option[ContextDecomposition] = {
      val callsInRule = rule.body.predCalls
      val candidateMatching = callsInRule zip rootsToMerge
      val assignmentsByVar: Map[Var, Set[Set[Var]]] = varAssignmentFromMatching(candidateMatching)
      assignmentsByVar.find(_._2.size >= 2) match {
        case Some(pair) =>
          // FIXME: Do we have to implement speculative merging where we match even if we have duplicate targets, but record this as missing equality constraint in the pure constraints of the resulting extension type?
          logger.debug(s"Can't match ${rule.body} against $rootsToMerge: ${pair._1} has to be assigned to all of ${pair._2.mkString(", ")}")
          None
        case None =>
          logger.debug(s"Will put contexts rooted in ${rootsToMerge.mkString(",")} under new root node labeled by $rule")
          Some(mergeRoots(decomp, rootsToMerge, rule, pred, assignmentsByVar.mapValues(_.head)))
      }
    }

    private def varAssignmentFromMatching(matched: Seq[(PredCall, ContextPredCall)]) = {
      val varAssignmentSeq = for {
        (call, root) <- matched
        (arg, varLabel) <- (call.args, root.subst.toSeq).zipped
      } yield (arg, varLabel)
      val assignmentsByVar: Map[Var, Set[Set[Var]]] = varAssignmentSeq.groupBy(_._1).map {
        case (k, vs) => (k, vs.map(_._2).toSet)
      }
      assignmentsByVar
    }

    private def mergeRoots(decomp: ContextDecomposition, rootsToMerge: Seq[ContextPredCall], rule: RuleBody, pred: Predicate, assignmentsByVar: Map[Var, Set[Var]]): ContextDecomposition = {
      val (tifsToMerge, unchangedTifs) = decomp.parts.partition(tif => rootsToMerge.contains(tif.root))
      logger.debug(s"Roots that were matched: $rootsToMerge")
      logger.debug(s"Will apply $rule to merge:\n${tifsToMerge.mkString("\n")}")
      logger.debug(s"Merge based on variable assignment $assignmentsByVar")
      val subst = Substitution(rule.body.freeVars map assignmentsByVar)
      val newRoot = ContextPredCall(pred, subst)
      val concatenatedLeaves = tifsToMerge.flatMap(_.calls)
      val mergedUsageInfo = integrateUsageInfo(tifsToMerge, Set(newRoot) ++ concatenatedLeaves)
      val mergedPureConstraints = tifsToMerge.map(_.pureConstraints).reduceLeft(_ compose _)
      val ctxAfterMerging = EntailmentContext(newRoot, concatenatedLeaves, mergedUsageInfo, mergedPureConstraints)
      ContextDecomposition(unchangedTifs + ctxAfterMerging)
    }

    private def integrateUsageInfo(ctxsToMerge: Set[EntailmentContext], nodeLabelsInMergedInterface: Iterable[ContextPredCall]) = {
      // Because we're not doing any unification, the usage info can simply be merged -- no propagation of unification results necessary
      val mergedUsageInfo = ctxsToMerge.map(_.usageInfo).reduceLeft(VarUsageByLabel.merge)
      VarUsageByLabel.restrictToSubstitutionsInLabels(mergedUsageInfo, nodeLabelsInMergedInterface)
    }

  }

}
