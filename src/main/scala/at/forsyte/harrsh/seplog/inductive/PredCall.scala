package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{PtrExpr, Renaming, Var}

/**
  * Created by jens on 2/28/17.
  */
//trait PredicateCall extends SepLogAtom {
//
//}

/**
  * Inductive spatial predicate, whose semantics is given by a SID
  * @param name Name of the predicate
  * @param args Nonempty sequence of arguments
  */
//case class SpatialUserDefPredCall(name : String, args : Seq[PtrExpr]) extends PredicateCall {
case class PredCall(name : String, args : Seq[PtrExpr]) extends SepLogAtom /*PredicateCall*/ {
  override def toStringWithVarNames(names: VarNaming) = name + "(" + args.map(_.toStringWithVarNames(names)).mkString(",") + ")"

  override def isSpatial: Boolean = true

  override def isPure: Boolean = false

  override def isSymbolicHeap: Boolean = true

  override def toSymbolicHeap: Option[SymbolicHeap] = Some(SymbolicHeap(Seq.empty, Seq(this)))

  override def renameVars(f: Renaming): PredCall = copy(args = args map (_.renameVars(f)))

  def getVars : Set[Var] = (args flatMap (_.getVar)).toSet
}