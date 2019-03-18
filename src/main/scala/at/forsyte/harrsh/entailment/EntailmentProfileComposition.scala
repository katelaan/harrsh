package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, Predicate, RuleBody, RichSid}
import at.forsyte.harrsh.util.Combinators

object EntailmentProfileComposition extends HarrshLogging {

  object composeAll {

    // TODO: Get rid of the second parameter once we use sets. (The new parameters then simply are the union of the params of the constituting profiles)
    def apply(sid: RichSid, profiles: Seq[EntailmentProfile], newOrderedParams: Seq[Var]): Option[EntailmentProfile] = {
      logger.debug(s"Will compose the following ${profiles.size} profiles:\n" + profiles.mkString("\n"))
      assert(profiles forall (p => p.decomps.forall(_.isConsistentWithFocus(sid))))
      if (profiles.forall(_.nonEmpty)) {
        val composedDecomps = allPossibleDecompCompositions(sid, profiles map (_.decomps))
        if (composedDecomps.isEmpty) {
          logger.debug(s"Composed profiles are contradictory => No composition result")
          None
        } else {
          val res = EntailmentProfile(composedDecomps, newOrderedParams)
          logger.debug(s"Profile composition result:\n${if (res.nonEmpty) res.decomps.mkString("\n") else "empty (sink state)"}")
          Some(res)
        }
      } else {
        logger.debug(s"Short-circuiting (propagating sink state) profile composition of:\n${profiles.mkString("\n")}")
        Some(EntailmentProfile(Set.empty, newOrderedParams))
      }
    }

    private def allPossibleDecompCompositions(sid: RichSid, decompsBySource: Seq[Set[ContextDecomposition]]): Set[ContextDecomposition] = {
      if (decompsBySource.tail.isEmpty) {
        decompsBySource.head
      } else {
        val partialComposition = allPossibleDecompCompositions(sid, decompsBySource.head, decompsBySource.tail.head)
        allPossibleDecompCompositions(sid, partialComposition +: decompsBySource.drop(2))
      }
    }

    private def allPossibleDecompCompositions(sid: RichSid, fst: Set[ContextDecomposition], snd: Set[ContextDecomposition]): Set[ContextDecomposition] = {
      logger.debug(s"Will compose $fst and $snd")
      val res = for {
        decompOfFst <- fst
        decompOfSnd <- snd
        combined <- decompOfFst.compositionOptions(sid, decompOfSnd)
      } yield combined
      logger.debug(s"Preliminary decomps in profile composition:\n${if (res.nonEmpty) res.mkString("\n") else "empty (sink state)"}")
      res
    }

  }

  object mergeUsingNonProgressRules {
    // TODO: Simplify application of non-progress rules to profiles?

    def apply(profile: EntailmentProfile, sid: RichSid): EntailmentProfile = {
      val newDecomps = for {
        decomp <- profile.decomps
        merged <- useNonProgressRulesToMergeContexts(decomp, sid)
      } yield merged
      EntailmentProfile(newDecomps, profile.orderedParams)
    }

    def useNonProgressRulesToMergeContexts(decomp: ContextDecomposition, sid: RichSid): Seq[ContextDecomposition] = {
      val nonProgressRules = sid.empClosedNonProgressRules
      if (nonProgressRules.nonEmpty) {
        logger.debug(s"Will try to apply non-progress rules to contexts in decomposition. Rules to consider: ${nonProgressRules.map(pair => s"${pair._1.defaultCall} <= ${pair._2}")}")
        mergeWithZeroOrMoreRuleApplications(decomp, nonProgressRules, sid)
      }
      else {
        val msg = if (!decomp.isMultiPartDecomposition) "Singleton decomposition => No merging via non-progress rules possible"
        else "The SID does not contain any non-progress rule. Will return decomposition as is."
        logger.debug(msg)
        Seq(decomp)
      }
    }

    private def mergeWithZeroOrMoreRuleApplications(decomp: ContextDecomposition, rules: Set[(Predicate, RuleBody)], sid: RichSid): Seq[ContextDecomposition] = {
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

    private def applyAllPossibleRulesInMerge(decomp: ContextDecomposition, rules: Set[(Predicate, RuleBody)]): Stream[ContextDecomposition] = {
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
          // FIXME: Do we have to implement speculative merging where we match even if we have duplicate targets, but record this as missing equality constraint in the pure constraints of the resulting decomposition?
          logger.debug(s"Can't match ${rule.body} against $rootsToMerge: ${pair._1} has to be assigned to all of ${pair._2.mkString(", ")}")
          None
        case None =>
          logger.debug(s"Will put contexts rooted in ${rootsToMerge.mkString(",")} under new root node labeled by $rule")
          mergeRoots(decomp, rootsToMerge, rule, pred, assignmentsByVar.mapValues(_.head))
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

    private def mergeRoots(decomp: ContextDecomposition, rootsToMerge: Seq[ContextPredCall], rule: RuleBody, pred: Predicate, assignmentsByVar: Map[Var, Set[Var]]): Option[ContextDecomposition] = {
      val (ctxsToMerge, unchangedCtxs) = decomp.parts.partition(ctx => rootsToMerge.contains(ctx.root))
      logger.debug(s"Roots that were matched: $rootsToMerge")
      logger.debug(s"Will apply $rule to merge:\n${ctxsToMerge.mkString("\n")}")
      logger.debug(s"Merge based on variable assignment $assignmentsByVar")
      val subst = Substitution(rule.body.freeVars map assignmentsByVar)
      val newRoot = ContextPredCall(pred, subst)
      val concatenatedLeaves = ctxsToMerge.flatMap(_.calls)
      val ctxAfterMerging = EntailmentContext(newRoot, concatenatedLeaves)

      for {
        restrictedConstraints <- decomp.constraints.restrictToNonPlaceholdersAnd(decomp.occurringLabels)
        mergedDecomp = ContextDecomposition(unchangedCtxs + ctxAfterMerging, restrictedConstraints)
        speculationUpdate = PureAtomUpdate(rule.body.pure, mergedDecomp.constraints.classes)
        withSpeculation <- mergedDecomp.updateSubst(speculationUpdate)
      } yield withSpeculation
    }

  }

}
