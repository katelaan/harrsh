package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

sealed trait TransitionETypes {
  def toTarget: Option[EntailmentAutomaton.State]
}

case class UnmatchableLocalAllocation(lab: SymbolicHeap) extends TransitionETypes with HarrshLogging {
  override def toTarget: Option[EntailmentAutomaton.State] = {
    logger.debug(s"No extension types for local allocation $lab => Return inconsistent state")
    Some(EntailmentAutomaton.State(Set.empty, lab.freeVars))
  }
}

case object InconsistentTransitionSources extends TransitionETypes with HarrshLogging {
  override def toTarget: Option[EntailmentAutomaton.State] = {
    logger.debug(s"Instantiation of transition sources is inconsistent => No target state")
    None
  }
}

case class ConsistentTransitionETypes(etypes: Seq[Set[ExtensionType]], lab: SymbolicHeap, sid: SID) extends TransitionETypes with HarrshLogging {

  override def toTarget: Option[EntailmentAutomaton.State] = {
    val targetETs = allPossibleETCompositions
    logger.debug(s"Target state:\n${if (targetETs.nonEmpty) targetETs.mkString("\n") else "empty (sink state)"}")
    Some(EntailmentAutomaton.State(targetETs, lab.freeVars))
  }

  private def allPossibleETCompositions: Set[ExtensionType] = {
    for {
      ets <- Combinators.choices(etypes map (_.toSeq)).toSet[Seq[ExtensionType]]
      combined <- tryToCombineETs(ets, lab)
    } yield combined
  }

  private def tryToCombineETs(ets: Seq[ExtensionType], lab: SymbolicHeap): Seq[ExtensionType] = {
    logger.debug(s"Trying to combine:\n${ets.map(_.parts.mkString("ET(", "\n  ", ")")).mkString("\n")}\nw.r.t. symbolic heap $lab...")
    val res = for {
      combined <- composeAll(ets).toSeq
      _ = logger.debug(s"Resulting parts of the ET:\n${combined.parts.mkString("\n")}")
      afterRuleApplications <- TransitionETypes.useNonProgressRulesToMergeTreeInterfaces(combined, sid)
      _ = {
        logger.debug(if (afterRuleApplications != combined) s"After applying non-progress rules:\n${afterRuleApplications.parts.mkString("\n")}" else "No non-progress rule can be applied.")
      }
      // Bound variables are not visible from outside the transition, so we remove them from the extension type
      restrictedToFreeVars <- restrictToFreeVars(afterRuleApplications)
      _ = logger.debug(s"After restriction to free variables:\n${restrictedToFreeVars.parts.mkString("\n")}")
      _ = assert(restrictedToFreeVars.boundVars.isEmpty, s"Bound vars remain after restriction to free vars: $restrictedToFreeVars")
      // FIXME: Also filter out types that lack names for back pointers
      // FIXME: What to check for the top-level predicates that don't have a root parameter annotation?
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

  private def composeAll(ets: Seq[ExtensionType]): Option[ExtensionType] = {
    if (ets.size <= 1) ets.headOption else {
      for {
        combinedHead <- ets.head compose ets.tail.head
        allComposed <- composeAll(combinedHead +: ets.drop(2))
      } yield allComposed
    }
  }

  private def restrictToFreeVars(etype: ExtensionType): Option[ExtensionType] = {
    logger.debug(s"Bound vars in result: ${etype.boundVars} (in the symbolic heap: ${lab.boundVars.mkString(",")})")
    assert(etype.boundVars.diff(lab.boundVars).isEmpty,
      s"Extension type $etype contains bound vars not in symbolic heap $lab")
    etype.dropVars(lab.boundVars.toSeq)
  }

}

object TransitionETypes extends HarrshLogging {

  def apply(src: Seq[EntailmentAutomaton.State], lab: SymbolicHeap, sid: SID): TransitionETypes = {
    val instantiatedETs = InstantiatedSourceStates(src, lab)
    if (instantiatedETs.isConsistent) {
      val local = LocalETs(lab, sid)
      combineLocalAndSourceEtypes(local, instantiatedETs, lab, sid)
    } else {
      InconsistentTransitionSources
    }
  }

  private def combineLocalAndSourceEtypes(local: LocalETs, instantiatedETs: InstantiatedSourceStates, lab: SymbolicHeap, sid: SID) = {
    if (local.areDefined) {
      ConsistentTransitionETypes(local +: instantiatedETs, lab, sid)
    } else {
      UnmatchableLocalAllocation(lab)
    }
  }

  def useNonProgressRulesToMergeTreeInterfaces(etype: ExtensionType, sid: SID): Seq[ExtensionType] = {
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

  private def mergeWithRules(etype: ExtensionType, rules: Seq[(Predicate, RuleBody)], sid: SID): Seq[ExtensionType] = {
    val afterMerging = for {
      etypeAfterRuleApplication <- applyAllPossibleRules(etype, rules)
      afterAdditionalApplications <- mergeWithRules(etypeAfterRuleApplication, rules, sid)
    } yield afterAdditionalApplications

    if (afterMerging.isEmpty) {
      Seq(etype)
    } else {
      afterMerging
    }
  }

  private def applyAllPossibleRules(etype: ExtensionType, rules: Seq[(Predicate, RuleBody)]): Stream[ExtensionType] = {
    rules.toStream.flatMap(rule => applyRule(etype, rule._2, rule._1))
  }

  private def applyRule(extensionType: ExtensionType, rule: RuleBody, pred: Predicate): Option[ExtensionType] = {
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

  private def tryMergeGivenRoots(extensionType: ExtensionType, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[NodeLabel]): Option[ExtensionType] = {
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

  private def tryArgumentMatching(extensionType: ExtensionType, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[NodeLabel]): Option[ExtensionType] = {
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

  private def mergeRoots(extensionType: ExtensionType, rootsToMerge: Seq[NodeLabel], rule: RuleBody, pred: Predicate, assignmentsByVar: Map[Var, Set[Var]]): ExtensionType = {
    val (tifsToMerge, unchangedTifs) = extensionType.parts.partition(tif => rootsToMerge.contains(tif.root))
    logger.debug(s"Roots that were matched: $rootsToMerge")
    logger.debug(s"Will apply $rule to merge:\n${tifsToMerge.mkString("\n")}")
    logger.debug(s"Merge based on variable assignment $assignmentsByVar")
    val subst = Substitution(rule.body.freeVars map assignmentsByVar)
    val newRoot = RuleNodeLabel(pred, rule, subst)
    val concatenatedLeaves = tifsToMerge.flatMap(_.leaves)
    val mergedUsageInfo = integrateUsageInfo(tifsToMerge, Set(newRoot) ++ concatenatedLeaves)
    val mergedPureConstraints = tifsToMerge.map(_.pureConstraints).reduceLeft(_ compose _)
    val tifAfterMerging = TreeInterface(newRoot, concatenatedLeaves, mergedUsageInfo, mergedPureConstraints)
    ExtensionType(unchangedTifs + tifAfterMerging)
  }

  private def integrateUsageInfo(tifsToMerge: Set[TreeInterface], nodeLabelsInMergedInterface: Iterable[NodeLabel]) = {
    // Because we're not doing any unification, the usage info can simply be merged -- no propagation of unification results necessary
    val mergedUsageInfo = tifsToMerge.map(_.usageInfo).reduceLeft(VarUsageByLabel.merge)
    VarUsageByLabel.restrictToSubstitutionsInLabels(mergedUsageInfo, nodeLabelsInMergedInterface)
  }
}