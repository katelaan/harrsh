package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}

sealed trait InstantiatedSourceStates {
  def isConsistent: Boolean = this match {
    case InconsistentInstantiatedSourceStates => false
    case ConsistentInstantiatedSourceStates(instantiatedEtypesByState) => true
  }

  def +:(other: LocalETs) : Seq[Set[ExtensionType]]
}

case object InconsistentInstantiatedSourceStates extends InstantiatedSourceStates {

  def +:(other: LocalETs) : Seq[Set[ExtensionType]] = throw new IllegalStateException("Can't process inconsistent source states")

}

case class ConsistentInstantiatedSourceStates(instantiatedEtypesByState: Seq[Set[ExtensionType]]) extends InstantiatedSourceStates {

  def +:(other: LocalETs) : Seq[Set[ExtensionType]] = {
    other match {
      case NoLocalETypes => instantiatedEtypesByState
      case ETypesOfLocalAtoms(ets) => ets +: instantiatedEtypesByState
    }
  }

}

object InstantiatedSourceStates extends HarrshLogging {

  def apply(src: Seq[EntailmentAutomaton.State], lab: SymbolicHeap): InstantiatedSourceStates = {
    val instantiatedETs = instantiatedSourceStates(src, lab)
    val consistentEts = instantiatedETs map (_ filterNot isInconsistent)
    if (consistentEts forall (_.nonEmpty))
      ConsistentInstantiatedSourceStates(consistentEts)
    else
      InconsistentInstantiatedSourceStates
  }

  private def instantiatedSourceStates(src: Seq[EntailmentAutomaton.State], lab: SymbolicHeap): Seq[Set[ExtensionType]] = {
    val instantiatedETs = (src, lab.predCalls).zipped.map(instantiateETsForCall)
    for {
      (src, renamed, call) <- (src, instantiatedETs, lab.predCalls).zipped
    } {
      logger.debug(s"Process pred call $call: Instantiated source state $src to $renamed")
    }
    instantiatedETs
  }

  private def instantiateETsForCall(state: EntailmentAutomaton.State, call: PredCall): Set[ExtensionType] = {
    val EntailmentAutomaton.State(ets, params) = state
    val callUpdate: SubstitutionUpdate = v => {
      // If v is the i-th free variable of the predicate, replace it with the i-th argument of the call;
      // otherwise, return the variable as is
      val fvIx = params.indexOf(v)
      if (fvIx >= 0) Set(call.args(fvIx)) else Set(v)
    }
    ets.map(_.updateSubst(callUpdate))
  }

  private def isInconsistent(extensionType: ExtensionType): Boolean = {
    allocsNull(extensionType) || doubleAlloc(extensionType)
  }

  private def allocsNull(et: ExtensionType): Boolean = {
    et.rootParamSubsts exists Var.containsNull
  }

  private def doubleAlloc(et: ExtensionType): Boolean = {
    val doublyAlloced = for {
      (k, vs) <- et.rootParamSubsts.groupBy(vs => vs).toStream
      if vs.size > 1
    } yield k
    doublyAlloced.nonEmpty
  }

}