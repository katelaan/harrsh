package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{PtrExpr, Renaming, Var, VarNaming}

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait PureAtom extends SepLogAtom with HarrshLogging {

  override def isSpatial = false

  override def isPure = true

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this), Seq.empty, Seq.empty))

  override def renameVars(f: Renaming): PureAtom = this match {
    case PtrEq(l, r) => PtrEq(l.renameVars(f), r.renameVars(f))
    case PtrNEq(l, r) => PtrNEq(l.renameVars(f), r.renameVars(f))
  }

  override def getVars : Set[Var] = this match {
    case PtrEq(l, r) => l.getVar union r.getVar
    case PtrNEq(l, r) => l.getVar union r.getVar
  }

  def getVarsWithNull : Set[Var] = this match {
    case PtrEq(l, r) => Set(l.getVarOrZero, r.getVarOrZero)
    case PtrNEq(l, r) => Set(l.getVarOrZero, r.getVarOrZero)
  }

  def isPointerComparison = true

  def ordered : PureAtom = this match {
    case eq@PtrEq(l, r) => if (l < r) eq else PtrEq(r,l)
    case neq@PtrNEq(l, r) => if (l < r) neq else PtrNEq(r,l)
  }

}

//sealed trait EqualityPureAtom extends PureAtom

case class PtrEq(l : PtrExpr, r : PtrExpr) extends /*Equality*/PureAtom {
  override def toStringWithVarNames(names: VarNaming) = l.toStringWithVarNames(names) + " \u2248 " + r.toStringWithVarNames(names)
}

case class PtrNEq(l : PtrExpr, r : PtrExpr) extends /*Equality*/PureAtom {
  override def toStringWithVarNames(names: VarNaming) = l.toStringWithVarNames(names) + " \u2249 " + r.toStringWithVarNames(names)
}