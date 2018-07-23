package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{PtrExpr, Renaming, Var, VarNaming}

/**
  * Created by jkatelaa on 10/3/16.
  */
case class PureAtom(l: PtrExpr, r: PtrExpr, isEquality: Boolean) extends SepLogAtom with HarrshLogging {

  override def isSpatial = false

  override def isPure = true

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this), Seq.empty, Seq.empty))

  override def renameVars(f: Renaming): PureAtom = PureAtom(l.renameVars(f), r.renameVars(f), isEquality)

  override def getVars : Set[Var] = l.getNonNullVar union r.getNonNullVar

  def getVarsWithNull : Set[Var] = Set(l.getVarOrZero, r.getVarOrZero)

  def comparesFree: Boolean = getVars.forall(_.isFree)

  def isPointerComparison = true

  def ordered : PureAtom = if (l < r) this else PureAtom(r, l, isEquality)

  override def toStringWithVarNames(names: VarNaming): String = {
    val op = if (isEquality) " \u2248 " else " \u2249 "
    l.toStringWithVarNames(names) + op + r.toStringWithVarNames(names)
  }

}

object PtrEq {
  def apply(l : Var, r : Var) : PureAtom = PureAtom(PtrExpr(l), PtrExpr(r), true)
  def apply(l : PtrExpr, r : PtrExpr) : PureAtom = PureAtom(l, r, true)
}

object PtrNEq {
  def apply(l : Var, r : Var) : PureAtom = PureAtom(PtrExpr(l), PtrExpr(r), false)
  def apply(l : PtrExpr, r : PtrExpr) : PureAtom = PureAtom(l, r, false)
}