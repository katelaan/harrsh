package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.HeapAutomaton.Transition
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive._

sealed trait TargetProfile {
  def getTransition(body: SymbolicHeap, head: String): Option[Transition[EntailmentProfile]]
}

case object InconsistentProfile extends TargetProfile with HarrshLogging {
  override def getTransition(body: SymbolicHeap, head: String): Option[Transition[EntailmentProfile]] = {
    logger.debug(s"Transition undefined (inconsistent source instantiations or inconsistent composition)")
    None
  }
}

case class ConsistentTargetProfile(localProfile: EntailmentProfile, renamedSourceStates: RenamedSourceStates, targetProfile: EntailmentProfile) extends TargetProfile {
  override def getTransition(body: SymbolicHeap, head: String): Option[Transition[EntailmentProfile]] = {
    Some(Transition(
      renamedSourceStates.renamedProfilesByState,
      body,
      Some(localProfile),
      head,
      targetProfile))
  }
}

object TargetProfile extends HarrshLogging {

  def apply(src: Seq[EntailmentProfile], lab: SymbolicHeap, sid: RichSid): TargetProfile = {
    val renamedProfiles = RenamedSourceStates(sid, src, lab)
    if (renamedProfiles.isConsistent) {
      val local = LocalProfile(lab, sid)
      assert(local.isConsistentWithFocus(sid),
        s"Local profile $local contains inconsistent decompositions")
      combineLocalAndSourceProfiles(local, renamedProfiles, lab, sid)
    } else {
      InconsistentProfile
    }
  }

  private def combineLocalAndSourceProfiles(local: EntailmentProfile, instantiatedProfiles: RenamedSourceStates, lab: SymbolicHeap, sid: RichSid) = {
    val profiles = local +: instantiatedProfiles
    composeAndRestrictToFVs(profiles, lab, sid) match {
      case None =>
        logger.debug("Profiles are incompatible. No composition result.")
        InconsistentProfile
      case Some(composed) =>
        logger.debug("Composition result: " + composed)
        ConsistentTargetProfile(local, instantiatedProfiles, composed)
    }
  }

  private def composeAndRestrictToFVs(profiles: Seq[EntailmentProfile], lab: SymbolicHeap, sid: RichSid): Option[EntailmentProfile] = {
    EntailmentProfileComposition.composeAndDropVars(profiles, lab.boundVars.toSet, sid, performEmpClosure = true)
  }

}
