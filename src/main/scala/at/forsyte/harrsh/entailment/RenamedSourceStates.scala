package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{PredCall, RichSid, SymbolicHeap}

sealed trait RenamedSourceStates {
  def isConsistent: Boolean = this match {
    case InconsistentRenamedSourceStates => false
    case ConsistentRenamedSourceStates(_) => true
  }

  val renamedProfilesByState: Seq[EntailmentProfile]

  def +:(other: EntailmentProfile) : Seq[EntailmentProfile]
}

case object InconsistentRenamedSourceStates extends RenamedSourceStates {

  override val renamedProfilesByState: Seq[Nothing] = Seq.empty

  def +:(other: EntailmentProfile) : Seq[EntailmentProfile] = throw new IllegalStateException("Can't process inconsistent source states")

}

case class ConsistentRenamedSourceStates(override val renamedProfilesByState: Seq[EntailmentProfile]) extends RenamedSourceStates {

  def +:(localProfile: EntailmentProfile) : Seq[EntailmentProfile] = {
    localProfile +: renamedProfilesByState
  }

}

object RenamedSourceStates extends HarrshLogging {
  
  def apply(sid: RichSid, src: Seq[EntailmentProfile], lab: SymbolicHeap): RenamedSourceStates = {
    val renamedProfiles = renamedSourceStates(sid, src, lab)
    if (renamedProfiles.exists(_.isEmpty)) {
      // Renaming introduced inconsistency (e.g. null alloc)
      InconsistentRenamedSourceStates
    }
    else {
      ConsistentRenamedSourceStates(renamedProfiles map (_.get))
    }

  }

  private def renamedSourceStates(sid: RichSid, src: Seq[EntailmentProfile], lab: SymbolicHeap): Seq[Option[EntailmentProfile]] = {
    val instantiatedETs = (src, lab.predCalls).zipped.map(renameProfileForCall(sid, _, _))
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    instantiatedETs
  }

  private def renameProfileForCall(sid: RichSid, state: EntailmentProfile, call: PredCall): Option[EntailmentProfile] = {
    state.renameOrFail(sid, call.args)
  }

}