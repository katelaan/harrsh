package slex.slex.slsyntax

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait PtrExpr {

  override def toString = this match {
    case NullPtr() => "null"
    case PtrVar(id) => id
  }

}

case class NullPtr() extends PtrExpr

/**
  * So far single-successor assumption => Need not specify a selector/field
  * @param id
  */
case class PtrVar(id : String) extends PtrExpr