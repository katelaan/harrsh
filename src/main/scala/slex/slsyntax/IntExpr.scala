package slex.slsyntax

import slex.smtsyntax.SmtExpr
import slex.smtsyntax.SmtExpr._

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait IntExpr {

  def collectIdents : Set[String] = this match {
    case IntConst(n) => Set()
    case IntVar(id) => Set(id)
    case Plus(l, r) => l.collectIdents union r.collectIdents
    case Minus(l, r) => l.collectIdents union r.collectIdents
  }

  def toSmtExpr : SmtExpr = this match {
    case IntConst(n) => ""+n
    case IntVar(id) => id
    case Plus(l, r) => plusExpr(l.toSmtExpr, r.toSmtExpr)
    case Minus(l, r) => minusExpr(l.toSmtExpr, r.toSmtExpr)
  }

}

case class IntConst(n : Int) extends IntExpr {
  override def toString = "" + n
}

case class IntVar(id : String) extends IntExpr {
  override def toString = id
}

case class Plus(l : IntExpr, r : IntExpr) extends IntExpr {
  override def toString = l + "+" + r
}

case class Minus(l : IntExpr, r : IntExpr) extends IntExpr {
  override def toString = l + "-" + r
}
