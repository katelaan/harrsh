package slex.smtsyntax

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait SmtFormula {

  protected def app(args : String*) = args.mkString("(", " ", ")")

}

case class DeclareConst(name : String, sort : String) extends SmtFormula {

  override def toString = app("declare-const", name, sort)

}

case class DeclareFun(name : String, args : Seq[String], res : String) extends SmtFormula {

  override def toString = app("declare-fun", name, args.mkString("(", " ", ")"), res)

}

case class Assert(expr : SmtExpr) extends SmtFormula {
  override def toString = app("assert", expr.toString)
}

case class CheckSat() extends SmtFormula {
  override def toString = app("check-sat")
}

case class GetModel() extends SmtFormula {
  override def toString = app("get-model")
}

case class RawFormula(formula : String) extends SmtFormula {

  override def toString = app(formula)

}
