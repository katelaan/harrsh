package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators
import at.forsyte.harrsh.util.Combinators.inCase

sealed trait TargetProfile {
  def get: Option[EntailmentProfile]
}

case class UnmatchableLocalAllocation(lab: SymbolicHeap) extends TargetProfile with HarrshLogging {
  override def get: Option[EntailmentProfile] = {
    logger.debug(s"No profile for local allocation of $lab => Return inconsistent state")
    Some(EntailmentProfile(Set.empty, lab.freeVars))
  }
}

case object InconsistentTransitionSources extends TargetProfile with HarrshLogging {
  override def get: Option[EntailmentProfile] = {
    logger.debug(s"Instantiation of transition sources is inconsistent => No target state")
    None
  }
}

case class ConsistentTargetProfile(targetProfile: EntailmentProfile) extends TargetProfile {
  override def get: Option[EntailmentProfile] = Some(targetProfile)
}

object TargetProfile extends HarrshLogging {

  def apply(src: Seq[EntailmentProfile], lab: SymbolicHeap, sid: RichSid): TargetProfile = {
    val renamedProfiles = RenamedSourceStates(sid, src, lab)
    if (renamedProfiles.isConsistent) {
      val local = LocalProfile(lab, sid)
      combineLocalAndSourceProfiles(local, renamedProfiles, lab, sid)
    } else {
      InconsistentTransitionSources
    }
  }

  private def combineLocalAndSourceProfiles(local: EntailmentProfile, instantiatedProfiles: RenamedSourceStates, lab: SymbolicHeap, sid: RichSid) = {
    if (local.nonEmpty) {
      val profiles = local +: instantiatedProfiles
      val composed = composeAndForget(profiles, lab, sid)
      logger.debug("Composition result: " + composed)
      ConsistentTargetProfile(composed)
    } else {
      UnmatchableLocalAllocation(lab)
    }
  }

  private def composeAndForget(profiles: Seq[EntailmentProfile], lab: SymbolicHeap, sid: RichSid): EntailmentProfile = {
    val composed = EntailmentProfileComposition.composeAll(sid, profiles, lab.freeVars ++ lab.boundVars)
    logger.debug(s"Target profile after initial composition:\n$composed")
    val processComposedProfile = (inCase(sid.hasEmptyBaseRules)(empClosure(sid))
      andThen inCase(sid.hasRecursiveRulesWithoutPointers)(mergeUsingNonProgressRules(sid))
      andThen restrictToFreeVars(lab)
      andThen dropNonviable(sid))
    processComposedProfile(composed)
  }

  private def empClosure(sid: RichSid)(profile: EntailmentProfile): EntailmentProfile = {
    logger.debug("Will compute emp-closure")
    profile.copy(decomps = profile.decomps.flatMap(empClosureOfDecomp(sid)))
  }

  private def empClosureOfDecomp(sid: RichSid)(decomp: ContextDecomposition): Set[ContextDecomposition] = {
    assert(!decomp.isInconsistent(sid),
      "Trying to compute emp-closure of inconsistent decomposition " + decomp)
    val empClosureByCtx: Seq[Set[(EntailmentContext, Set[PureAtom])]] = decomp.parts.toSeq map empClosureOfContext(sid)
    for {
      closureOption: Seq[(EntailmentContext, Set[PureAtom])] <- Combinators.choices(empClosureByCtx)
      _ = logger.debug(s"Considering emp-closure for $decomp:\n${closureOption.map(p => p._1 + " with new pure constraints " + p._2).mkString(",\n")}")
      (newCtxs, constraintsByCtx) = closureOption.unzip
      newConstraints = constraintsByCtx.flatten
      allConstraints = decomp.pureConstraints.addToMissingUnlessEnsured(newConstraints)
      _ = logger.debug(s"Updated pure constraints from ${decomp.pureConstraints} to $allConstraints")
      if allConstraints.closure.isConsistent
      _ = logger.debug("Will check for double allocation...")
      newClasses = allConstraints.closure.classes.toSeq
      if !doubleAlloc(decomp, newClasses)
      update = SubstitutionUpdate.fromUnification(newClasses)
      updated = decomp.copy(parts = newCtxs.toSet, pureConstraints = allConstraints).updateSubst(update).toPlaceholderNormalForm
      _ = logger.debug("Emp-closure is consistent. Will retain updated decomposition\n" + updated)
      _ = assert(!updated.isInconsistent(sid),
        s"$decomp updated to inconsistent decomposition $updated via new constraints $newConstraints")
    } yield updated
  }

  private def doubleAlloc(decomp: ContextDecomposition, eqClasses: Seq[Set[Var]]): Boolean = {
    eqClasses exists {
      vs =>
        val originalClasses = decomp.usageInfo.filter(_._1.intersect(vs).nonEmpty)
        originalClasses.count(_._2 == VarAllocated) > 1
    }
  }

  private def empClosureOfContext(sid: RichSid)(ctx: EntailmentContext): Set[(EntailmentContext, Set[PureAtom])] = {

    def empClosureOfCalls(calls: Set[ContextPredCall], remainingCalls: Set[ContextPredCall] = Set.empty, constraintsSoFar: Set[PureAtom] = Set.empty) : Set[(Set[ContextPredCall], Set[PureAtom])] = {
      if (calls.isEmpty) {
        Set((remainingCalls, constraintsSoFar))
      } else {
        val (hd, tl) = (calls.head, calls.tail)
        empClosureOfCalls(tl, remainingCalls + hd, constraintsSoFar) ++
          empClosureAfterRemoval(hd, tl, remainingCalls, constraintsSoFar)
      }
    }

    def empClosureAfterRemoval(callToRemove: ContextPredCall, unprocessedCalls: Set[ContextPredCall], remainingCalls: Set[ContextPredCall], constraintsSoFar: Set[PureAtom]) : Set[(Set[ContextPredCall], Set[PureAtom])] = {
      if (sid.canBeEmpty(callToRemove.pred)) {
        val constraintOptions = constraintOptionsForCal(callToRemove)
        val closureOfUnprocessedCalls = empClosureOfCalls(unprocessedCalls, remainingCalls, constraintsSoFar)
        for {
          (remainingAfterClosure, constraintsAfterClosure) <- closureOfUnprocessedCalls
          constraint <- constraintOptions
        } yield (remainingAfterClosure, constraintsAfterClosure ++ constraint)
      } else {
        Set.empty
      }
    }

    def constraintOptionsForCal(call: ContextPredCall) = {
      val update: Map[Var, Var] = (call.freeVarSeq zip call.subst.toSeq.map(_.head)).toMap
      for {
        option <- sid.constraintOptionsForEmptyModels(call.pred)
      } yield option.map(atom => PureAtom(update(atom.l), update(atom.r), atom.isEquality))
    }

    for {
      (callsAfterClosure, constraintsOfClosure) <- empClosureOfCalls(ctx.calls)
    } yield (EntailmentContext(ctx.root, callsAfterClosure), constraintsOfClosure)
  }

  private def mergeUsingNonProgressRules(sid: RichSid)(profile: EntailmentProfile): EntailmentProfile = {
    logger.debug("Will try to apply non-progress rules to intermediate profile " + profile)
    val merged = EntailmentProfileComposition.mergeUsingNonProgressRules(profile, sid)
    if (merged != profile) {
      logger.debug(s"Updated target profile by applying non-progress rules:\n${merged.decomps.mkString("\n")}")
    }
    merged
  }

  private def restrictToFreeVars(lab: SymbolicHeap)(profile: EntailmentProfile): EntailmentProfile = {
    // Bound variables are not visible from outside the transition, so we remove them from the results
    // Note that some decompositions may be removed in the process
    if (lab.boundVars.nonEmpty) {
      logger.debug(s"Will forget bound vars ${lab.boundVars} in intermediate profile $profile")
      profile.forget(lab.boundVars.toSet)
    } else {
      profile
    }
  }

  private def dropNonviable(sid: RichSid)(profile: EntailmentProfile) = {
    // TODO: Option to turn on/off viability checks (perhaps also multiple variants of viability checks)
    logger.debug("Will drop non-viable decompositions in intermediate profile " + profile)
    val viable = profile.dropNonViableDecompositions(sid)
    if (viable != profile) {
      logger.debug(s"After dropping at least one nonviable decomposition:\n${profile.decomps.mkString("\n")}")
    }
    viable
  }
}
