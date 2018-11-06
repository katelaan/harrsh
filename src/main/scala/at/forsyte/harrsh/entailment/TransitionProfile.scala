package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

sealed trait TransitionProfile {
  def toTarget: Option[EntailmentAutomaton.CutProfile]
}

case class UnmatchableLocalAllocation(lab: SymbolicHeap) extends TransitionProfile with HarrshLogging {
  override def toTarget: Option[EntailmentAutomaton.CutProfile] = {
    logger.debug(s"No cut profile for local allocation of $lab => Return inconsistent state")
    Some(EntailmentAutomaton.CutProfile(Set.empty, lab.freeVars))
  }
}

case object InconsistentTransitionSources extends TransitionProfile with HarrshLogging {
  override def toTarget: Option[EntailmentAutomaton.CutProfile] = {
    logger.debug(s"Instantiation of transition sources is inconsistent => No target state")
    None
  }
}

case class ConsistentTransitionProfile(etypes: Seq[Set[TreeCuts]], lab: SymbolicHeap, sid: SID) extends TransitionProfile with HarrshLogging {

  override def toTarget: Option[EntailmentAutomaton.CutProfile] = {
    val targetETs = allPossibleETCompositions
    logger.debug(s"Target state:\n${if (targetETs.nonEmpty) targetETs.mkString("\n") else "empty (sink state)"}")
    Some(EntailmentAutomaton.CutProfile(targetETs, lab.freeVars))
  }

  private def allPossibleETCompositions: Set[TreeCuts] = {
    for {
      ets <- Combinators.choices(etypes map (_.toSeq)).toSet[Seq[TreeCuts]]
      combined <- tryToCombineETs(ets)
    } yield combined
  }

  private def tryToCombineETs(ets: Seq[TreeCuts]): Seq[TreeCuts] = {
    logger.debug(s"Trying to combine:\n${ets.map(_.parts.mkString("ET(", "\n  ", ")")).mkString("\n")}")
    val res = for {
      (combined, i) <- composeAll(ets).zipWithIndex
      _ = logger.debug(s"Composed ET #${i+1}:\n${combined.parts.mkString("\n")}")
      afterRuleApplications <- TransitionProfile.useNonProgressRulesToMergeTreeInterfaces(combined, sid)
      _ = {
        logger.debug(if (afterRuleApplications != combined) s"After applying non-progress rules:\n${afterRuleApplications.parts.mkString("\n")}" else "No non-progress rule can be applied.")
      }
      // Bound variables are not visible from outside the transition, so we remove them from the extension type
      restrictedToFreeVars <- restrictToFreeVars(afterRuleApplications)
      _ = logger.debug(s"After restriction to free variables:\n${restrictedToFreeVars.parts.mkString("\n")}")
      _ = assert(restrictedToFreeVars.boundVars.isEmpty, s"Bound vars remain after restriction to free vars: $restrictedToFreeVars")
      // FIXME: Also filter out types that lack names for back pointers
      // FIXME: What to check for the top-level predicates that don't have a root parameter annotation?
      // At the end of the composition, every root parameter needs to have a name because without having names for the roots,
      // we can never extend the extension type to a full unfolding: There's no way to compose if you don't have names.
      if restrictedToFreeVars.hasNamesForAllRootParams
      _ = logger.debug("Extension type has names for all roots. Will keep if viable.")
      if restrictedToFreeVars.isViable(sid)
      _ = logger.debug("Extension type is viable, will become part of target state.")
    } yield restrictedToFreeVars

    if (res.isEmpty) {
      logger.debug("Could not combine ETs.")
    }

    res
  }

  private def composeAll(ets: Seq[TreeCuts]): Seq[TreeCuts] = {
    if (ets.size <= 1) ets else {
      for {
        combinedHead <- ets.head compositionOptions ets.tail.head
        allComposed <- composeAll(combinedHead +: ets.drop(2))
      } yield allComposed
    }
  }

  private def restrictToFreeVars(etype: TreeCuts): Option[TreeCuts] = {
    logger.debug(s"Bound vars in result: ${etype.boundVars} (in the symbolic heap: ${lab.boundVars.mkString(",")})")
    assert(etype.boundVars.diff(lab.boundVars).isEmpty,
      s"Extension type $etype contains bound vars not in symbolic heap $lab")
    etype.dropVars(lab.boundVars.toSeq)
  }

}

object TransitionProfile extends HarrshLogging {

  def apply(src: Seq[EntailmentAutomaton.CutProfile], lab: SymbolicHeap, sid: SID): TransitionProfile = {
    val instantiatedETs = InstantiatedSourceStates(src, lab)
    if (instantiatedETs.isConsistent) {
      val local = LocalCutProfile(lab, sid)
      combineLocalAndSourceEtypes(local, instantiatedETs, lab, sid)
    } else {
      InconsistentTransitionSources
    }
  }

  private def combineLocalAndSourceEtypes(local: LocalCutProfile, instantiatedETs: InstantiatedSourceStates, lab: SymbolicHeap, sid: SID) = {
    if (local.areDefined) {
      ConsistentTransitionProfile(local +: instantiatedETs, lab, sid)
    } else {
      UnmatchableLocalAllocation(lab)
    }
  }

  def useNonProgressRulesToMergeTreeInterfaces(etype: TreeCuts, sid: SID): Seq[TreeCuts] = {
    val nonProgressRules = sid.rulesWithoutPointers
    if (nonProgressRules.nonEmpty && !etype.representsSingleTree) {
      logger.debug(s"Will try to apply non-progress rules to merge trees in extension type. Rules to consider: ${nonProgressRules.map(pair => s"${pair._1.defaultCall} <= ${pair._2}")}")
      mergeWithRules(etype, nonProgressRules, sid)
    }
    else {
      val msg = if (etype.representsSingleTree) "Extension type represents a single tree => No merging of trees via non-progress rules possible"
      else "The SID does not contain any non-progress rule. Will return extension type as is."
      logger.debug(msg)
      Seq(etype)
    }
  }

  private def mergeWithRules(etype: TreeCuts, rules: Seq[(Predicate, RuleBody)], sid: SID): Seq[TreeCuts] = {
    val afterMerging = for {
      etypeAfterRuleApplication <- applyAllPossibleRulesInMerge(etype, rules)
      afterAdditionalApplications <- mergeWithRules(etypeAfterRuleApplication, rules, sid)
    } yield afterAdditionalApplications

    if (afterMerging.isEmpty) {
      Seq(etype)
    } else {
      afterMerging
    }
  }

  private def applyAllPossibleRulesInMerge(etype: TreeCuts, rules: Seq[(Predicate, RuleBody)]): Stream[TreeCuts] = {
    rules.toStream.flatMap(rule => applyRuleInMerge(etype, rule._2, rule._1))
  }

  private def applyRuleInMerge(extensionType: TreeCuts, rule: RuleBody, pred: Predicate): Option[TreeCuts] = {
    assert(!rule.hasPointer)
    val callsInRule = rule.body.predCalls
    val roots = extensionType.parts map (_.root)
    if (callsInRule.size > roots.size)
      // The rule contains more calls than the ETypes has parts => Rule can't be applicable
      None
    else {
      // FIXME: More efficient choice of possible pairings for matching (don't brute force over all seqs)
      val possibleMatchings = Combinators.allSeqsWithoutRepetitionOfLength(callsInRule.length, roots)
      possibleMatchings.toStream.flatMap(tryMergeGivenRoots(extensionType, rule, pred, _)).headOption
    }
  }

  private def tryMergeGivenRoots(extensionType: TreeCuts, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[PredicateNodeLabel]): Option[TreeCuts] = {
    val callsInRule = rule.body.predCalls
    assert(rootsToMerge.size == callsInRule.size)
    if (predicatesMatch(callsInRule, rootsToMerge)) {
      tryArgumentMatching(extensionType, rule, pred, rootsToMerge)
    }
    else {
      None
    }
  }

  private def predicatesMatch(calls: Seq[PredCall], labels: Seq[NodeLabel]): Boolean = {
    (calls, labels).zipped forall {
      case (call, label) => call.name == label.pred.head
    }
  }

  private def tryArgumentMatching(extensionType: TreeCuts, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[PredicateNodeLabel]): Option[TreeCuts] = {
    val callsInRule = rule.body.predCalls
    val candidateMatching = callsInRule zip rootsToMerge
    val assignmentsByVar: Map[Var, Set[Set[Var]]] = varAssignmentFromMatching(candidateMatching)
    assignmentsByVar.find(_._2.size >= 2) match {
      case Some(pair) =>
        // FIXME: Do we have to implement speculative merging where we match even if we have duplicate targets, but record this as missing equality constraint in the pure constraints of the resulting extension type?
        logger.debug(s"Can't match ${rule.body} against $rootsToMerge: ${pair._1} has to be assigned to all of ${pair._2.mkString(", ")}")
        None
      case None =>
        logger.debug(s"Will put tree interfaces rooted in ${rootsToMerge.mkString(",")} under new root node labeled by $rule")
        Some(mergeRoots(extensionType, rootsToMerge, rule, pred, assignmentsByVar.mapValues(_.head)))
    }
  }

  private def varAssignmentFromMatching(matched: Seq[(PredCall, NodeLabel)]) = {
    val varAssignmentSeq = for {
      (call, root) <- matched
      (arg, varLabel) <- (call.args, root.subst.toSeq).zipped
    } yield (arg, varLabel)
    val assignmentsByVar: Map[Var, Set[Set[Var]]] = varAssignmentSeq.groupBy(_._1).map {
      case (k, vs) => (k, vs.map(_._2).toSet)
    }
    assignmentsByVar
  }

  private def mergeRoots(extensionType: TreeCuts, rootsToMerge: Seq[PredicateNodeLabel], rule: RuleBody, pred: Predicate, assignmentsByVar: Map[Var, Set[Var]]): TreeCuts = {
    val (tifsToMerge, unchangedTifs) = extensionType.parts.partition(tif => rootsToMerge.contains(tif.root))
    logger.debug(s"Roots that were matched: $rootsToMerge")
    logger.debug(s"Will apply $rule to merge:\n${tifsToMerge.mkString("\n")}")
    logger.debug(s"Merge based on variable assignment $assignmentsByVar")
    val subst = Substitution(rule.body.freeVars map assignmentsByVar)
    val newRoot = PredicateNodeLabel(pred, subst)
    val concatenatedLeaves = tifsToMerge.flatMap(_.leaves)
    val mergedUsageInfo = integrateUsageInfo(tifsToMerge, Set(newRoot) ++ concatenatedLeaves)
    val mergedPureConstraints = tifsToMerge.map(_.pureConstraints).reduceLeft(_ compose _)
    val tifAfterMerging = TreeCut(newRoot, concatenatedLeaves, mergedUsageInfo, mergedPureConstraints)
    TreeCuts(unchangedTifs + tifAfterMerging)
  }

  private def integrateUsageInfo(tifsToMerge: Set[TreeCut], nodeLabelsInMergedInterface: Iterable[NodeLabel]) = {
    // Because we're not doing any unification, the usage info can simply be merged -- no propagation of unification results necessary
    val mergedUsageInfo = tifsToMerge.map(_.usageInfo).reduceLeft(VarUsageByLabel.merge)
    VarUsageByLabel.restrictToSubstitutionsInLabels(mergedUsageInfo, nodeLabelsInMergedInterface)
  }
}
