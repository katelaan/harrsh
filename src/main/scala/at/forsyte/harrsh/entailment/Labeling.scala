package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.{PredCall, PureAtom}
import at.forsyte.harrsh.seplog.{FreeVar, Var}

case class Labeling(toMap: Map[FreeVar,Set[Var]]) extends AnyVal {

  def apply(key: FreeVar): Set[Var] = toMap(key)

  def propagate(predCall: PredCall) : Labeling = ???

  def toAtoms : Iterable[PureAtom] = for {
    (k,vs) <- toMap
    v <- vs
  } yield k =:= v

}