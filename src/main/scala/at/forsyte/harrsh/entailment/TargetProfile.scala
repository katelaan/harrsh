package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive._

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

  private def localAllocConsistent(local: Option[EntailmentProfile]): Boolean = {
    local match {
      case Some(profile) =>
        // Return true iff there is at least one way to decompose the local allocation
        profile.nonEmpty
      case None =>
        // No local allocation (i.e., no pointer or pure atoms) at all
        // This can only happen for SIDs outside the BTW fragment, i.e., for SIDs with "generalized progress"
        // TODO: Explicitly turn on/off support for "generalized progress"?
        true
    }
  }

  private def combineLocalAndSourceProfiles(local: Option[EntailmentProfile], instantiatedProfiles: RenamedSourceStates, lab: SymbolicHeap, sid: RichSid) = {
    if (localAllocConsistent(local)) {
      val profiles = local +: instantiatedProfiles
      val composed = composeAndForget(profiles, lab, sid)
      ConsistentTargetProfile(composed)
    } else {
      UnmatchableLocalAllocation(lab)
    }
  }

  private def composeAndForget(profiles: Seq[EntailmentProfile], lab: SymbolicHeap, sid: RichSid): EntailmentProfile = {
    val composed = ComposeProfiles.composeAll(sid, profiles, lab.freeVars ++ lab.boundVars)
    logger.debug(s"Target profile after initial composition:\n${composed.decomps.mkString("\n")}")
    val processComposedProfile = mergeUsingNonProgressRules(sid)_ andThen restrictToFreeVars(lab) andThen dropNonviable(sid)
    processComposedProfile(composed)
  }

  private def mergeUsingNonProgressRules(sid: RichSid)(profile: EntailmentProfile): EntailmentProfile = {
    // TODO: Only call this if we actually have a generalized-progress SID
    val merged = ComposeProfiles.mergeUsingNonProgressRules(profile, sid)
    if (merged != profile) {
      logger.debug(s"Updated target profile by applying non-progress rules:\n${merged.decomps.mkString("\n")}")
    }
    merged
  }

  private def restrictToFreeVars(lab: SymbolicHeap)(profile: EntailmentProfile): EntailmentProfile = {
    // Bound variables are not visible from outside the transition, so we remove them from the results
    // Note that some decompositions may be removed in the process
    val restricted = profile.forget(lab.boundVars.toSet)
    if (restricted.decomps != profile.decomps) {
      logger.debug(s"Updated target profile after forgetting bound vars:\n${restricted.decomps.mkString("\n")}")
    }
    restricted
  }

  private def dropNonviable(sid: RichSid)(profile: EntailmentProfile) = {
    // TODO: Option to turn on/off viability checks (perhaps also multiple variants of viability checks)
    val viable = profile.dropNonViableDecompositions(sid)
    if (viable != profile) {
      logger.debug(s"After dropping at least one nonviable decomposition:\n${profile.decomps.mkString("\n")}")
    }
    viable
  }
}
