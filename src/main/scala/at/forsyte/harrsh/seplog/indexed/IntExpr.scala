package at.forsyte.harrsh.seplog.indexed

import at.forsyte.harrsh.seplog.{Expr, Renaming}
import at.forsyte.harrsh.smtsyntax.SmtExpr
import at.forsyte.harrsh.smtsyntax.SmtExpr._

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait IntExpr extends Expr {

  def getVars : Set[String] = this match {
    case IntConst(n) => Set()
    case IntVar(id) => Set(id)
    case Plus(l, r) => l.getVars union r.getVars
    case Minus(l, r) => l.getVars union r.getVars
  }

  def toSmtExpr : SmtExpr = this match {
    case IntConst(n) => ""+n
    case IntVar(id) => id
    case Plus(l, r) => plusExpr(l.toSmtExpr, r.toSmtExpr)
    case Minus(l, r) => minusExpr(l.toSmtExpr, r.toSmtExpr)
  }

  def constantEval : Option[Int] = this match {
    case IntConst(n) => Some(n)
    case IntVar(id) => None
    case Plus(l, r) => for {
      cl <- l.constantEval
      cr <- r.constantEval
    } yield cl + cr
    case Minus(l, r) => for {
      cl <- l.constantEval
      cr <- r.constantEval
    } yield cl - cr
  }

  def renameVars(f : Renaming): IntExpr = this match {
    case c : IntConst => c
    case IntVar(id) => IntVar(f(id))
    case Plus(l, r) => Plus(l.renameVars(f), r.renameVars(f))
    case Minus(l, r) => Minus(l.renameVars(f), r.renameVars(f))
  }

}

case class IntConst(n : Int) extends IntExpr {
  override def toString = "" + n
}

case class IntVar(id : String) extends IntExpr {
  override def toString = id
}

case class Plus(l : IntExpr, r : IntExpr) extends IntExpr {
  override def toString = "(" + l + "+" + r + ")"
}

case class Minus(l : IntExpr, r : IntExpr) extends IntExpr {
  override def toString = "(" + l + "-" + r + ")"
}
