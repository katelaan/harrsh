package slex.slex.slsyntax

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait SepLogFormula {

}

case class IxEq(l : IntExpr, r : IntExpr) extends SepLogFormula {
  override def toString = l + " \u2248 " + r
}

case class IxGT(l : IntExpr, r : IntExpr) extends SepLogFormula {
  override def toString = l + " > " + r
}

case class IxLT(l : IntExpr, r : IntExpr) extends SepLogFormula {
  override def toString = l + " < " + r
}

case class IntNEq(l : IntExpr, r : IntExpr) extends SepLogFormula {
  override def toString = l + " \u2249 " + r
}

case class PtrEq(l : PtrExpr, r : PtrExpr) extends SepLogFormula {
  override def toString = l + " \u2248 " + r
}

case class PtrNEq(l : PtrExpr, r : PtrExpr) extends SepLogFormula {
  override def toString = l + " \u2249 " + r
}

case class Neg(phi : SepLogFormula) extends SepLogFormula {
  override def toString = "(\u00ac " + phi + ")"
}

case class And(phi : SepLogFormula, psi : SepLogFormula) extends SepLogFormula {
  override def toString = "(" + phi + " \u2227 " + psi + ")"
}

case class Or(phi : SepLogFormula, psi : SepLogFormula) extends SepLogFormula {
  override def toString = "(" + phi + " \u2228 " + psi + ")"
}

case class Exists(varid : String, phi : SepLogFormula) extends SepLogFormula {
  override def toString = "(\u2203 "  + varid + " . " + phi + ")"
}

case class SepCon(phi : SepLogFormula, psi : SepLogFormula) extends SepLogFormula {
  override def toString = "(" + phi + " * " + psi + ")"
}

case class Emp() extends SepLogFormula {
  override def toString = "emp"
}

case class PointsTo(from : PtrExpr, to : PtrExpr) extends SepLogFormula {
  override def toString = from + " \u2192 " + to
}

case class LSeg(from : PtrExpr, to : PtrExpr, lngth : IntExpr) extends SepLogFormula {
  override def toString = "lseg(" + from + ", " + to + ", " +  lngth + ")"
}
