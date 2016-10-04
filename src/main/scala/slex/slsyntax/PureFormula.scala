package slex.slsyntax

import slex.smtsyntax.SmtExpr
import slex.smtsyntax.SmtExpr._

/**
  * Created by jkatelaa on 10/3/16.
  */
trait PureFormula extends SepLogFormula {

  override def isPure = true

  override def isSpatial = false

  def toSmtExpr : SmtExpr
}

case class PureNeg(phi : PureFormula) extends PureFormula {

  override def toString = "(\u00ac " + phi + ")"

  override def isSymbolicHeap: Boolean = false

  override def toSymbolicHeap: Option[SymbolicHeap] = None

  override def toSmtExpr: SmtExpr = notExpr(phi.toSmtExpr)
}

case class PureAnd(phi : PureFormula, psi : PureFormula) extends PureFormula {

  override def toString = "(" + phi + " \u2227 " + psi + ")"

  override def isSymbolicHeap: Boolean = phi.isSymbolicHeap && psi.isSymbolicHeap

  override def toSymbolicHeap: Option[SymbolicHeap] = SymbolicHeap.combineHeaps(phi.toSymbolicHeap, psi.toSymbolicHeap)

  override def toSmtExpr: SmtExpr = andExpr(phi.toSmtExpr, psi.toSmtExpr)
}

case class PureOr(phi : PureFormula, psi : PureFormula) extends PureFormula {

  override def toString = "(" + phi + " \u2228 " + psi + ")"

  override def isSymbolicHeap: Boolean = false

  override def toSymbolicHeap: Option[SymbolicHeap] = None

  override def toSmtExpr: SmtExpr = orExpr(phi.toSmtExpr, psi.toSmtExpr)
}

object PureFormula {

  def collectIdentifiers(phi : PureFormula) : Set[String] = phi match {
    case PureNeg(phi) => collectIdentifiers(phi)
    case PureAnd(phi, psi) => collectIdentifiers(phi) union collectIdentifiers(psi)
    case PureOr(phi, psi) => collectIdentifiers(phi) union collectIdentifiers(psi)
    case a : PureAtom => a match {
      case True() => Set()
      case IxEq(l, r) => l.collectIdents ++ r.collectIdents
      case IxGT(l, r) => l.collectIdents ++ r.collectIdents
      case IxLT(l, r) => l.collectIdents ++ r.collectIdents
      case IxLEq(l, r) => l.collectIdents ++ r.collectIdents
      case IxGEq(l, r) => l.collectIdents ++ r.collectIdents
      case IntNEq(l, r) => l.collectIdents ++ r.collectIdents
      case PtrEq(l, r) => Set() ++ l.getIdent ++ r.getIdent
      case PtrNEq(l, r) => Set() ++ l.getIdent ++ r.getIdent
    }
  }

}
