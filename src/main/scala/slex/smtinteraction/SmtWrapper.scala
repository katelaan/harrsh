package slex.smtinteraction

import slex.smtsyntax.SmtFormula

/**
  * Created by jkatelaa on 9/30/16.
  */
trait SmtWrapper {

  def runSmtQuery(query : Seq[SmtFormula]) : String

}
