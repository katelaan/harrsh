package slex.seplog

/**
  * Created by jkatelaa on 9/30/16.
  */
case class SepLogDef(left : SepLogFormula, right : SepLogFormula) {

  override def toString = left + " <== " + right

}
