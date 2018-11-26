package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

sealed trait TransitionProfile {
  def toTarget: Option[EntailmentProfile]
}

case class UnmatchableLocalAllocation(lab: SymbolicHeap) extends TransitionProfile with HarrshLogging {
  override def toTarget: Option[EntailmentProfile] = {
    logger.debug(s"No profile for local allocation of $lab => Return inconsistent state")
    Some(EntailmentProfile(Set.empty, lab.freeVars))
  }
}

case object InconsistentTransitionSources extends TransitionProfile with HarrshLogging {
  override def toTarget: Option[EntailmentProfile] = {
    logger.debug(s"Instantiation of transition sources is inconsistent => No target state")
    None
  }
}

case class ConsistentTransitionProfile(decompsBySource: Seq[Set[ContextDecomposition]], lab: SymbolicHeap, sid: SID) extends TransitionProfile with HarrshLogging {

  override def toTarget: Option[EntailmentProfile] = {
    val targetProfiles = allPossibleDecompCompositions
    logger.debug(s"Target state:\n${if (targetProfiles.nonEmpty) targetProfiles.mkString("\n") else "empty (sink state)"}")
    Some(EntailmentProfile(targetProfiles, lab.freeVars))
  }

  private def allPossibleDecompCompositions: Set[ContextDecomposition] = {
    for {
      decomp <- Combinators.choices(decompsBySource map (_.toSeq)).toSet[Seq[ContextDecomposition]]
      combined <- tryToCombineDecomps(decomp)
    } yield combined
  }

  private def tryToCombineDecomps(decomps: Seq[ContextDecomposition]): Seq[ContextDecomposition] = {
    logger.debug(s"Trying to combine:\n${decomps.map(_.parts.mkString("Decomp(", "\n  ", ")")).mkString("\n")}")
    val res = for {
      (combined, i) <- composeAll(decomps).zipWithIndex
      _ = logger.debug(s"Composed Decomp #${i+1}:\n${combined.parts.mkString("\n")}")
      afterRuleApplications <- TransitionProfile.useNonProgressRulesToMergeTreeInterfaces(combined, sid)
      _ = {
        logger.debug(if (afterRuleApplications != combined) s"After applying non-progress rules:\n${afterRuleApplications.parts.mkString("\n")}" else "No non-progress rule can be applied.")
      }
      // Bound variables are not visible from outside the transition, so we remove them from the decomposition
      restrictedToFreeVars <- restrictToFreeVars(afterRuleApplications)
      _ = logger.debug(s"After restriction to free variables:\n${restrictedToFreeVars.parts.mkString("\n")}")
      _ = assert(restrictedToFreeVars.boundVars.isEmpty, s"Bound vars remain after restriction to free vars: $restrictedToFreeVars")
      // FIXME: Also filter out types that lack names for back pointers?
      // FIXME: What to check for the top-level predicates that don't have a root parameter annotation?
      // At the end of the composition, every root parameter needs to have a name because without having names for the roots,
      // we can never extend the decomposition to a full unfolding: There's no way to compose if you don't have names.
      if restrictedToFreeVars.hasNamesForAllRootParams
      _ = logger.debug("Decomposition has names for all roots. Will keep if viable.")
      if restrictedToFreeVars.isViable(sid)
      _ = logger.debug("Decomposition is viable, will become part of target state.")
    } yield restrictedToFreeVars

    if (res.isEmpty) {
      logger.debug("Could not combine decompositions.")
    }

    res
  }

  private def composeAll(decomps: Seq[ContextDecomposition]): Seq[ContextDecomposition] = {
    if (decomps.size <= 1) decomps else {
      for {
        combinedHead <- decomps.head compositionOptions decomps.tail.head
        allComposed <- composeAll(combinedHead +: decomps.drop(2))
      } yield allComposed
    }
  }

  private def restrictToFreeVars(decomp: ContextDecomposition): Option[ContextDecomposition] = {
    logger.debug(s"Bound vars in result: ${decomp.boundVars} (in the symbolic heap: ${lab.boundVars.mkString(",")})")
    assert(decomp.boundVars.diff(lab.boundVars).isEmpty,
      s"Decomposition $decomp contains bound vars not in symbolic heap $lab")
    decomp.dropVars(lab.boundVars.toSeq)
  }

}

object TransitionProfile extends HarrshLogging {

  def apply(src: Seq[EntailmentProfile], lab: SymbolicHeap, sid: SID): TransitionProfile = {
    val instantiatedETs = InstantiatedSourceStates(src, lab)
    if (instantiatedETs.isConsistent) {
      val local = LocalProfile(lab, sid)
      combineLocalAndSourceEtypes(local, instantiatedETs, lab, sid)
    } else {
      InconsistentTransitionSources
    }
  }

  private def combineLocalAndSourceEtypes(local: LocalProfile, instantiatedProfiles: InstantiatedSourceStates, lab: SymbolicHeap, sid: SID) = {
    if (local.areDefined) {
      ConsistentTransitionProfile(local +: instantiatedProfiles, lab, sid)
    } else {
      UnmatchableLocalAllocation(lab)
    }
  }

  def useNonProgressRulesToMergeTreeInterfaces(etype: ContextDecomposition, sid: SID): Seq[ContextDecomposition] = {
    val nonProgressRules = sid.rulesWithoutPointers
    if (nonProgressRules.nonEmpty && !etype.isSingletonDecomposition) {
      logger.debug(s"Will try to apply non-progress rules to contexts in decomposition. Rules to consider: ${nonProgressRules.map(pair => s"${pair._1.defaultCall} <= ${pair._2}")}")
      mergeWithRules(etype, nonProgressRules, sid)
    }
    else {
      val msg = if (etype.isSingletonDecomposition) "Decomposition represents a single tree => No merging of trees via non-progress rules possible"
      else "The SID does not contain any non-progress rule. Will return decomposition as is."
      logger.debug(msg)
      Seq(etype)
    }
  }

  private def mergeWithRules(etype: ContextDecomposition, rules: Seq[(Predicate, RuleBody)], sid: SID): Seq[ContextDecomposition] = {
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

  private def applyAllPossibleRulesInMerge(etype: ContextDecomposition, rules: Seq[(Predicate, RuleBody)]): Stream[ContextDecomposition] = {
    rules.toStream.flatMap(rule => applyRuleInMerge(etype, rule._2, rule._1))
  }

  private def applyRuleInMerge(extensionType: ContextDecomposition, rule: RuleBody, pred: Predicate): Option[ContextDecomposition] = {
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

  private def tryMergeGivenRoots(extensionType: ContextDecomposition, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[PredicateNodeLabel]): Option[ContextDecomposition] = {
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

  private def tryArgumentMatching(extensionType: ContextDecomposition, rule: RuleBody, pred: Predicate, rootsToMerge: Seq[PredicateNodeLabel]): Option[ContextDecomposition] = {
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

  private def mergeRoots(extensionType: ContextDecomposition, rootsToMerge: Seq[PredicateNodeLabel], rule: RuleBody, pred: Predicate, assignmentsByVar: Map[Var, Set[Var]]): ContextDecomposition = {
    val (tifsToMerge, unchangedTifs) = extensionType.parts.partition(tif => rootsToMerge.contains(tif.root))
    logger.debug(s"Roots that were matched: $rootsToMerge")
    logger.debug(s"Will apply $rule to merge:\n${tifsToMerge.mkString("\n")}")
    logger.debug(s"Merge based on variable assignment $assignmentsByVar")
    val subst = Substitution(rule.body.freeVars map assignmentsByVar)
    val newRoot = PredicateNodeLabel(pred, subst)
    val concatenatedLeaves = tifsToMerge.flatMap(_.calls)
    val mergedUsageInfo = integrateUsageInfo(tifsToMerge, Set(newRoot) ++ concatenatedLeaves)
    val mergedPureConstraints = tifsToMerge.map(_.pureConstraints).reduceLeft(_ compose _)
    val tifAfterMerging = EntailmentContext(newRoot, concatenatedLeaves, mergedUsageInfo, mergedPureConstraints)
    ContextDecomposition(unchangedTifs + tifAfterMerging)
  }

  private def integrateUsageInfo(tifsToMerge: Set[EntailmentContext], nodeLabelsInMergedInterface: Iterable[NodeLabel]) = {
    // Because we're not doing any unification, the usage info can simply be merged -- no propagation of unification results necessary
    val mergedUsageInfo = tifsToMerge.map(_.usageInfo).reduceLeft(VarUsageByLabel.merge)
    VarUsageByLabel.restrictToSubstitutionsInLabels(mergedUsageInfo, nodeLabelsInMergedInterface)
  }
}
