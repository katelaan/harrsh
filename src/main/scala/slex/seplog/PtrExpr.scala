package slex.seplog

import slex.smtsyntax.SmtExpr

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait PtrExpr extends Expr {

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

  def <(other : PtrExpr) : Boolean = (this, other) match {
    case (NullPtr(), NullPtr()) => false
    case (NullPtr(), _) => true
    case (_, NullPtr()) => false
    case (PtrVar(l), PtrVar(r)) => l < r
  }

  def renameVars(f : String => String) : PtrExpr = this match {
    case n : NullPtr => n
    case PtrVar(id) => PtrVar(f(id))
  }

}

case class NullPtr() extends PtrExpr

/**
  * So far single-successor assumption => Need not specify a selector/field
  * @param id
  */
case class PtrVar(id : String) extends PtrExpr