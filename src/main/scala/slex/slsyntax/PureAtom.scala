package slex.slsyntax

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait PureAtom extends SepLogFormula {

  def isSpatial = false

  def isPure = true

  def isSymbolicHeap = true

  def toSymbolicHeap = Some(SymbolicHeap(Seq(this), Seq(), Seq()))

}

case class IxEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2248 " + r
}

case class IxGT(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " > " + r
}

case class IxLT(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " < " + r
}

case class IntNEq(l : IntExpr, r : IntExpr) extends PureAtom {
  override def toString = l + " \u2249 " + r
}

case class PtrEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toString = l + " \u2248 " + r
}

case class PtrNEq(l : PtrExpr, r : PtrExpr) extends PureAtom {
  override def toString = l + " \u2249 " + r
}
