package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.{NullConst, Renaming, Var}

/**
  * Created by jens on 2/28/17.
  */

/**
  * Inductive spatial predicate, whose semantics is given by a SID
  * @param name Name of the predicate
  * @param args Nonempty sequence of arguments
  */
case class PredCall(name : String, args : Seq[Var]) extends SepLogAtom {
  override def toStringWithVarNames(names: Naming) = name + "(" + args.map(names).mkString(",") + ")"

  override def isSpatial: Boolean = true

  override def isPure: Boolean = false

  override def isSymbolicHeap: Boolean = true

  override def toSymbolicHeap: Option[SymbolicHeap] = Some(SymbolicHeap(Seq.empty, Seq.empty, Seq(this), Var.freeNonNullVars(args)))

  override def renameVars(f: Renaming): PredCall = copy(args = args map (_.rename(f)))

  override def getNonNullVars : Set[Var] = args.toSet - NullConst
}