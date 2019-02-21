package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{PredCall, RichSid, SymbolicHeap}

sealed trait RenamedSourceStates {
  def isConsistent: Boolean = this match {
    case InconsistentRenamedSourceStates => false
    case ConsistentRenamedSourceStates(_) => true
  }

  def +:(other: Option[EntailmentProfile]) : Seq[EntailmentProfile]
}

case object InconsistentRenamedSourceStates extends RenamedSourceStates {

  def +:(other: Option[EntailmentProfile]) : Seq[EntailmentProfile] = throw new IllegalStateException("Can't process inconsistent source states")

}

case class ConsistentRenamedSourceStates(renamedProfilesByState: Seq[EntailmentProfile]) extends RenamedSourceStates {

  def +:(other: Option[EntailmentProfile]) : Seq[EntailmentProfile] = {
    other match {
      case None => renamedProfilesByState
      case Some(localProfile) => localProfile +: renamedProfilesByState
    }
  }

}

object RenamedSourceStates extends HarrshLogging {

  def apply(sid: RichSid, src: Seq[EntailmentProfile], lab: SymbolicHeap): RenamedSourceStates = {
    val renamedProfiles = renamedSourceStates(sid, src, lab)
    if (renamedProfiles forall (_.nonEmpty))
      ConsistentRenamedSourceStates(renamedProfiles)
    else
      InconsistentRenamedSourceStates
  }

  private def renamedSourceStates(sid: RichSid, src: Seq[EntailmentProfile], lab: SymbolicHeap): Seq[EntailmentProfile] = {
    val instantiatedETs = (src, lab.predCalls).zipped.map(renameProfileForCall(sid, _, _))
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    instantiatedETs
  }

  private def renameProfileForCall(sid: RichSid, state: EntailmentProfile, call: PredCall): EntailmentProfile = {
    state.rename(sid, call.args)
  }

}