package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}

sealed trait InstantiatedSourceStates {
  def isConsistent: Boolean = this match {
    case InconsistentInstantiatedSourceStates => false
    case ConsistentInstantiatedSourceStates(instantiatedEtypesByState) => true
  }

  def +:(other: LocalCutProfile) : Seq[Set[TreeCuts]]
}

case object InconsistentInstantiatedSourceStates extends InstantiatedSourceStates {

  def +:(other: LocalCutProfile) : Seq[Set[TreeCuts]] = throw new IllegalStateException("Can't process inconsistent source states")

}

case class ConsistentInstantiatedSourceStates(instantiatedEtypesByState: Seq[Set[TreeCuts]]) extends InstantiatedSourceStates {

  def +:(other: LocalCutProfile) : Seq[Set[TreeCuts]] = {
    other match {
      case NoLocalCutProfile => instantiatedEtypesByState
      case CutProfileOfLocalAtoms(ets) => ets +: instantiatedEtypesByState
    }
  }

}

object InstantiatedSourceStates extends HarrshLogging {

  def apply(src: Seq[EntailmentAutomaton.CutProfile], lab: SymbolicHeap): InstantiatedSourceStates = {
    val instantiatedETs = instantiatedSourceStates(src, lab)
    val consistentEts = instantiatedETs map (_ filterNot isInconsistent)
    if (consistentEts forall (_.nonEmpty))
      ConsistentInstantiatedSourceStates(consistentEts)
    else
      InconsistentInstantiatedSourceStates
  }

  private def instantiatedSourceStates(src: Seq[EntailmentAutomaton.CutProfile], lab: SymbolicHeap): Seq[Set[TreeCuts]] = {
    val instantiatedETs = (src, lab.predCalls).zipped.map(instantiateETsForCall)
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    instantiatedETs
  }

  private def instantiateETsForCall(state: EntailmentAutomaton.CutProfile, call: PredCall): Set[TreeCuts] = {
    val EntailmentAutomaton.CutProfile(ets, params) = state
    val callUpdate: SubstitutionUpdate = v => {
      // If v is the i-th free variable of the predicate, replace it with the i-th argument of the call;
      // otherwise, return the variable as is
      val fvIx = params.indexOf(v)
      if (fvIx >= 0) Set(call.args(fvIx)) else Set(v)
    }
    ets.map(_.updateSubst(callUpdate))
  }

  private def isInconsistent(extensionType: TreeCuts): Boolean = {
    allocsNull(extensionType) || doubleAlloc(extensionType)
  }

  private def allocsNull(et: TreeCuts): Boolean = {
    et.rootParamSubsts exists Var.containsNull
  }

  private def doubleAlloc(et: TreeCuts): Boolean = {
    val doublyAlloced = for {
      (k, vs) <- et.rootParamSubsts.groupBy(vs => vs).toStream
      if vs.size > 1
    } yield k
    doublyAlloced.nonEmpty
  }

}