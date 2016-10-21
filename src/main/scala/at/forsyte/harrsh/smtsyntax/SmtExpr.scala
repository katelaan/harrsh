package at.forsyte.harrsh.smtsyntax

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

object SmtExpr {

  def andExpr(left : SmtExpr, right : SmtExpr) = appExpr("and", left, right)

  def orExpr(left : SmtExpr, right : SmtExpr) = appExpr("or", left, right)

  def notExpr(arg : SmtExpr) = appExpr("not", arg)

  def impliesExpr(left : SmtExpr, right : SmtExpr) = appExpr("=>", left, right)

  def eqExpr(left : SmtExpr, right : SmtExpr) = appExpr("=", left, right)

  def neqExpr(left : SmtExpr, right : SmtExpr) = notExpr(appExpr("=", left, right))

  def ltExpr(left : SmtExpr, right : SmtExpr) = appExpr("<", left, right)

  def gtExpr(left : SmtExpr, right : SmtExpr) = appExpr(">", left, right)

  def geqExpr(left : SmtExpr, right : SmtExpr) = appExpr(">=", left, right)

  def leqExpr(left : SmtExpr, right : SmtExpr) = appExpr("<=", left, right)

  def plusExpr(left : SmtExpr, right : SmtExpr) = appExpr("+", left, right)

  def minusExpr(left : SmtExpr, right : SmtExpr) = appExpr("-", left, right)

}