package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}

sealed trait RenamedSourceStates {
  def isConsistent: Boolean = this match {
    case InconsistentRenamedSourceStates => false
    case ConsistentRenamedSourceStates(instantiatedEtypesByState) => true
  }

  def +:(other: LocalProfile) : Seq[Set[ContextDecomposition]]
}

case object InconsistentRenamedSourceStates extends RenamedSourceStates {

  def +:(other: LocalProfile) : Seq[Set[ContextDecomposition]] = throw new IllegalStateException("Can't process inconsistent source states")

}

case class ConsistentRenamedSourceStates(renamedProfilesByState: Seq[EntailmentProfile]) extends RenamedSourceStates {

  def +:(other: LocalProfile) : Seq[Set[ContextDecomposition]] = {
    other match {
      case NoLocalProfile => renamedProfilesByState.map(_.profile)
      case ProfileOfLocalAtoms(decomps) => decomps +: renamedProfilesByState.map(_.profile)
    }
  }

}

object RenamedSourceStates extends HarrshLogging {

  def apply(src: Seq[EntailmentProfile], lab: SymbolicHeap): RenamedSourceStates = {
    val renamedProfiles = renamedSourceStates(src, lab)
    if (renamedProfiles forall (_.nonEmpty))
      ConsistentRenamedSourceStates(renamedProfiles)
    else
      InconsistentRenamedSourceStates
  }

  private def renamedSourceStates(src: Seq[EntailmentProfile], lab: SymbolicHeap): Seq[EntailmentProfile] = {
    val instantiatedETs = (src, lab.predCalls).zipped.map(renameProfileForCall)
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    instantiatedETs
  }

  private def renameProfileForCall(state: EntailmentProfile, call: PredCall): EntailmentProfile = {
    state.rename(call.args)
  }

}