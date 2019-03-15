package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, NullConst, Var}
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

case object InconsistentProfile extends TargetProfile with HarrshLogging {
  override def get: Option[EntailmentProfile] = {
    logger.debug(s"Transition undefined (inconsistent source instantiations or inconsistent composition)")
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
      assert(local.decomps forall (d => d.hasConsistentConstraints && !d.isInconsistentWithFocus(sid)),
        s"Local profile $local contains inconsistent decompositions")
      combineLocalAndSourceProfiles(local, renamedProfiles, lab, sid)
    } else {
      InconsistentProfile
    }
  }

  private def combineLocalAndSourceProfiles(local: EntailmentProfile, instantiatedProfiles: RenamedSourceStates, lab: SymbolicHeap, sid: RichSid) = {
    if (local.nonEmpty) {
      val profiles = local +: instantiatedProfiles
      composeAndForget(profiles, lab, sid) match {
        case None =>
          logger.debug("Profiles are incompatible. No composition result.")
          InconsistentProfile
        case Some(composed) =>
          logger.debug("Composition result: " + composed)
          ConsistentTargetProfile(composed)
      }
    } else {
      UnmatchableLocalAllocation(lab)
    }
  }

  private def composeAndForget(profiles: Seq[EntailmentProfile], lab: SymbolicHeap, sid: RichSid): Option[EntailmentProfile] = {
    val composed = EntailmentProfileComposition.composeAll(sid, profiles, lab.freeVars ++ lab.boundVars)
    logger.debug(s"Target profile after initial composition:\n$composed")
    val processComposedProfile = (inCase(sid.hasEmptyBaseRules)(empClosure(sid))
      andThen filterOutInconsistentFocus(sid)
      andThen inCase(sid.hasRecursiveRulesWithoutPointers)(mergeUsingNonProgressRules(sid))
      andThen restrictToFreeVars(lab)
      andThen dropNonviable(sid))
    composed map processComposedProfile
  }

  private def filterOutInconsistentFocus(sid: RichSid)(profile: EntailmentProfile) = {
    profile.copy(decomps = profile.decomps.filterNot(_.isInconsistentWithFocus(sid)))
  }

  private def empClosure(sid: RichSid)(profile: EntailmentProfile): EntailmentProfile = {
    logger.debug("Will compute emp-closure")
    profile.copy(decomps = profile.decomps.flatMap(empClosureOfDecomp(sid)))
  }

  private def empClosureOfDecomp(sid: RichSid)(decomp: ContextDecomposition): Set[ContextDecomposition] = {
    val empClosureByCtx: Seq[Set[(EntailmentContext, Set[PureAtom])]] = decomp.parts.toSeq map empClosureOfContext(sid)
    for {
      closureOption: Seq[(EntailmentContext, Set[PureAtom])] <- Combinators.choices(empClosureByCtx)
      _ = logger.debug(s"Considering emp-closure for $decomp:\n${closureOption.map(p => p._1 + " with new pure constraints " + p._2).mkString(",\n")}")
      (newCtxs, pureConstraintsByCtx) = closureOption.unzip
      newPureAtoms = pureConstraintsByCtx.flatten
      newAtomsRelevantForSpeculation = newPureAtoms filterNot (atom => atom.isEquality && (PlaceholderVar.isPlaceholder(atom.l) || PlaceholderVar.isPlaceholder(atom.r)))
      _ = logger.debug{
        if (newAtomsRelevantForSpeculation.nonEmpty)
          s"Will speculate $newAtomsRelevantForSpeculation (unless already ensured)"
        else
          "No speculation necessary to apply the emp-closure."
      }
      constraintsWithNewSpeculation <- decomp.constraints.addToSpeculationUnlessEnsured(newAtomsRelevantForSpeculation)
      _ = logger.debug(s"Updated constraints from ${decomp.constraints} to $constraintsWithNewSpeculation")
      newEqualities = newPureAtoms.filter(_.isEquality).map(atom => Set(atom.l,atom.r))
      decompBeforeUpdate = ContextDecomposition(newCtxs.toSet, constraintsWithNewSpeculation)
      update = SubstitutionUpdate.fromSetsOfEqualVars(newEqualities)
      _ = logger.debug(s"Will apply update derived from ${newEqualities.mkString("{",", ","}")}, discarding the result in case of inconsistencies")
      // Note: If we set mayEnsureEqualities to true, we'd immediately lose the speculative equalities we just added
      newDecomp <- decompBeforeUpdate.updateSubst(update, mayEnsureEqualities = false)
      if newDecomp.hasConsistentConstraints
      if !newDecomp.isInconsistentWithFocus(sid)
      res = newDecomp.toPlaceholderNormalForm
      _ = logger.debug("Emp-closure is consistent. Will retain updated decomposition\n" + res)
    } yield res
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
      if (sid.hasEmptyModels(callToRemove.pred)) {
        val constraintOptions = constraintOptionsForCall(callToRemove)
        val closureOfUnprocessedCalls = empClosureOfCalls(unprocessedCalls, remainingCalls, constraintsSoFar)
        for {
          (remainingAfterClosure, constraintsAfterClosure) <- closureOfUnprocessedCalls
          constraint <- constraintOptions
        } yield (remainingAfterClosure, constraintsAfterClosure ++ constraint)
      } else {
        Set.empty
      }
    }

    def constraintOptionsForCall(call: ContextPredCall) = {
      val update: Map[Var, Var] = ((NullConst, NullConst) +: (call.freeVarSeq zip call.subst.toSeq.map(_.head))).toMap
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
