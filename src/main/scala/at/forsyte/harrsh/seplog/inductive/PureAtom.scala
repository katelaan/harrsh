package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.SlexLogging
import at.forsyte.harrsh.seplog.{PtrExpr, Renaming, Var}

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait PureAtom extends SepLogAtom with SlexLogging {

  override def isSpatial = false

  override def isPure = true

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this), Seq()))

  override def renameVars(f: Renaming): PureAtom = this match {
    case t : True => t
    case PtrEq(l, r) => PtrEq(l.renameVars(f), r.renameVars(f))
    case PtrNEq(l, r) => PtrNEq(l.renameVars(f), r.renameVars(f))
  }

  def getVars : Set[Var] = this match {
    case True() => Set()
    case PtrEq(l, r) => l.getVar union r.getVar // TODO Building so many sets is quite inefficient
    case PtrNEq(l, r) => l.getVar union r.getVar
  }

}

case class True() extends PureAtom {
  override def toStringWithVarNames(names: VarNaming): String = "true"
}

case class PtrEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toStringWithVarNames(names: VarNaming) = l.toStringWithVarNames(names) + " \u2248 " + r.toStringWithVarNames(names)
}

case class PtrNEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toStringWithVarNames(names: VarNaming) = l.toStringWithVarNames(names) + " \u2249 " + r.toStringWithVarNames(names)
}