package slex.slex.slsyntax

/**
  * Created by jkatelaa on 9/30/16.
  */
trait IntExpr {

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
