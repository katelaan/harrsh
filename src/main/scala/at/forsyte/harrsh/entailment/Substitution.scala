package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}

import scala.annotation.tailrec

case class Substitution(toSeq: Seq[Set[Var]]) extends AnyVal {

  override def toString: String = toSeq.map(_.mkString(",")).mkString("subst(", "; ", ")")

  def size: Int = toSeq.size

  def placeholders: Set[PlaceholderVar] = {
    for {
      vs <- toSeq.toSet[Set[Var]]
      v <- vs
      p <- PlaceholderVar.fromVar(v)
    } yield p
  }

  def orderedNonPlaceholders: Seq[Var] = {
    for {
      vs <- toSeq
      v <- vs.toSeq.sorted
      if !PlaceholderVar.isPlaceholder(v)
    } yield v
  }

  def nonNullVars: Set[Var] = toSeq.toSet.flatten - NullConst

  def boundVars: Set[BoundVar] = toSeq.flatten.collect{
    case bv: BoundVar => bv
  }.toSet

  def toAtoms(params: Seq[FreeVar]) : Seq[PureAtom] = for {
    (k,vs) <- params.zip(toSeq)
    v <- vs
  } yield k =:= v

  /**
    * Update this Labeling by applying f to all values in the labeling.
    * @param f Function to apply to each value in the range of the labeling
    */
  def update(f: ConstraintUpdater): Substitution = Substitution(toSeq map (f(_)))

}

object Substitution {
  def identity(fvs: Seq[Var]): Substitution = Substitution(fvs map (Set(_)))
}