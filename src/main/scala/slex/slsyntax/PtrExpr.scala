package slex.slsyntax

import slex.smtsyntax.SmtExpr

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait PtrExpr {

  override def toString = this match {
    case NullPtr() => "null"
    case PtrVar(id) => id
  }

  def getIdent : Option[String] = this match {
    case NullPtr() => Some("null") // FIXME [NULL-TREATMENT] Currently we handle null by declaring it as an ordinary constant. Might want to fix this at some point
    case PtrVar(id) => Some(id)
  }

  def toSmtExpr : SmtExpr = this match {
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