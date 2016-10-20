package slex.entailment

import slex.smtsyntax.{Assert, _}

/**
  * Created by jkatelaa on 9/30/16.
  */
object UFExample {


  lazy val Example : Seq[SmtCommand] = Seq(
    //(declare-const a Int)
    DeclareConst("a", "Int"),
    //(declare-fun f (Int Bool) Int)
    DeclareFun("f", Seq("Int", "Bool"), "Int"),
    //(assert (> a 10))
    Assert(appExpr(">", "a", "10")),
    //(assert (< (f a true) 100))
    Assert(appExpr("<", appExpr("f", "a", "true"), "100"))
    )

}
