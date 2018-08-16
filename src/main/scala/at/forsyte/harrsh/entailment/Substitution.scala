package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.{PredCall, PureAtom}
import at.forsyte.harrsh.seplog.FreeVar

// FIXME: Targets need to be arbitary vars, since we need to be able to map to null as well.
case class Substitution(toMap: Map[FreeVar,Set[FreeVar]]) extends AnyVal {

  def placeholders: Set[PlaceholderVar] = {
    for {
      vs: Set[FreeVar] <- toMap.values.toSet
      v <- vs
      p <- PlaceholderVar.fromVar(v)
    } yield p
  }

  def apply(key: FreeVar): Set[FreeVar] = toMap(key)

  def toAtoms : Iterable[PureAtom] = for {
    (k,vs) <- toMap
    v <- vs
  } yield k =:= v

  /**
    * Update this Labeling by applying f to all values in the labeling.
    * @param f Function to apply to each value in the range of the labeling
    */
  def update(f: FreeVar => Set[FreeVar]): Substitution = Substitution(toMap.map {
    case (k,vs) => (k, vs.flatMap(f))
  })

}

object Substitution {
  def identity(fvs: Seq[FreeVar]): Substitution = Substitution(fvs.map(fv => (fv,Set(fv))).toMap)
}