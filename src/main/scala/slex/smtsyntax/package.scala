package slex

import scala.language.implicitConversions

/**
  * Created by jkatelaa on 9/30/16.
  */
package object smtsyntax {

  implicit def stringToSmtExpr(s : String) : SmtExpr = BaseSmtExpr(s)

  def appExpr(args : SmtExpr*) = AppSmtExpr(args.toSeq)

}
