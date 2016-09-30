package slex.smtsyntax

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait SmtExpr {

  override def toString = this match {
    case AppSmtExpr(expr) => expr.mkString("(", " ", ")")
    case BaseSmtExpr(expr) => expr
  }

}

case class AppSmtExpr(expr : Seq[SmtExpr]) extends SmtExpr

case class BaseSmtExpr(expr : String) extends SmtExpr
