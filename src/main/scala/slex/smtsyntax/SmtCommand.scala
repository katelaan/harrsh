package slex.smtsyntax

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait SmtCommand {

  protected def app(args : String*) = args.mkString("(", " ", ")")

}

case class SetLogic(name : String) extends SmtCommand {

  override def toString = app("set-logic", name)

}

case class DeclareConst(name : String, sort : String) extends SmtCommand {

  override def toString = app("declare-const", name, sort)

}

case class DeclareFun(name : String, args : Seq[String], res : String) extends SmtCommand {

  override def toString = app("declare-fun", name, args.mkString("(", " ", ")"), res)

}

case class Assert(expr : SmtExpr) extends SmtCommand {
  override def toString = app("assert", expr.toString)
}

case class CheckSat() extends SmtCommand {
  override def toString = app("check-sat")
}

case class GetModel() extends SmtCommand {
  override def toString = app("get-model")
}

case class RawCommand(formula : String) extends SmtCommand {

  override def toString = app(formula)

}
