package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}

sealed trait InstantiatedSourceStates {
  def isConsistent: Boolean = this match {
    case InconsistentInstantiatedSourceStates => false
    case ConsistentInstantiatedSourceStates(instantiatedEtypesByState) => true
  }

  def +:(other: LocalProfile) : Seq[Set[ContextDecomposition]]
}

case object InconsistentInstantiatedSourceStates extends InstantiatedSourceStates {

  def +:(other: LocalProfile) : Seq[Set[ContextDecomposition]] = throw new IllegalStateException("Can't process inconsistent source states")

}

case class ConsistentInstantiatedSourceStates(instantiatedEtypesByState: Seq[Set[ContextDecomposition]]) extends InstantiatedSourceStates {

  def +:(other: LocalProfile) : Seq[Set[ContextDecomposition]] = {
    other match {
      case NoLocalProfile => instantiatedEtypesByState
      case ProfileOfLocalAtoms(ets) => ets +: instantiatedEtypesByState
    }
  }

}

object InstantiatedSourceStates extends HarrshLogging {

  def apply(src: Seq[EntailmentAutomaton.EntailmentProfile], lab: SymbolicHeap): InstantiatedSourceStates = {
    val instantiatedETs = instantiatedSourceStates(src, lab)
    val consistentEts = instantiatedETs map (_ filterNot isInconsistent)
    if (consistentEts forall (_.nonEmpty))
      ConsistentInstantiatedSourceStates(consistentEts)
    else
      InconsistentInstantiatedSourceStates
  }

  private def instantiatedSourceStates(src: Seq[EntailmentAutomaton.EntailmentProfile], lab: SymbolicHeap): Seq[Set[ContextDecomposition]] = {
    val instantiatedETs = (src, lab.predCalls).zipped.map(instantiateETsForCall)
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    instantiatedETs
  }

  private def instantiateETsForCall(state: EntailmentAutomaton.EntailmentProfile, call: PredCall): Set[ContextDecomposition] = {
    val EntailmentAutomaton.EntailmentProfile(ets, params) = state
    val callUpdate: SubstitutionUpdate = v => {
      // If v is the i-th free variable of the predicate, replace it with the i-th argument of the call;
      // otherwise, return the variable as is
      val fvIx = params.indexOf(v)
      if (fvIx >= 0) Set(call.args(fvIx)) else Set(v)
    }
    ets.map(_.updateSubst(callUpdate))
  }

  private def isInconsistent(extensionType: ContextDecomposition): Boolean = {
    allocsNull(extensionType) || doubleAlloc(extensionType)
  }

  private def allocsNull(et: ContextDecomposition): Boolean = {
    et.rootParamSubsts exists Var.containsNull
  }

  private def doubleAlloc(et: ContextDecomposition): Boolean = {
    val doublyAlloced = for {
      (k, vs) <- et.rootParamSubsts.groupBy(vs => vs).toStream
      if vs.size > 1
    } yield k
    doublyAlloced.nonEmpty
  }

}