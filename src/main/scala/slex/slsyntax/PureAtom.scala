package slex.slsyntax

import slex.smtsyntax.SmtExpr
import slex.smtsyntax.SmtExpr._

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait PureAtom extends SepLogFormula with PureFormula {

  override def isSpatial = false

  override def isPure = true

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this), Seq(), Seq()))

}

case class True() extends PureAtom {
  override def toString = "true"

  override def toSmtExpr: SmtExpr = "true"
}

case class False() extends PureAtom {
  override def toString = "false"

  override def toSmtExpr: SmtExpr = "false"
}

case class IxEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2248 " + r

  override def toSmtExpr: SmtExpr = eqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxGT(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " > " + r

  override def toSmtExpr: SmtExpr = gtExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxLT(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " < " + r

  override def toSmtExpr: SmtExpr = ltExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxLEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2264 " + r

  override def toSmtExpr: SmtExpr = leqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IxGEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2265 " + r

  override def toSmtExpr: SmtExpr = geqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class IntNEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2249 " + r

  override def toSmtExpr: SmtExpr = neqExpr(l.toSmtExpr, r.toSmtExpr)

}

case class PtrEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toString = l + " \u2248 " + r

  override def toSmtExpr: SmtExpr = eqExpr(l.toSmtExpr, r.toSmtExpr)
}

case class PtrNEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toString = l + " \u2249 " + r

  override def toSmtExpr: SmtExpr = neqExpr(l.toSmtExpr, r.toSmtExpr)
}
