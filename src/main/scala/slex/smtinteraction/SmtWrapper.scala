package slex.smtinteraction

import slex.smtsyntax.SmtCommand

/**
  * Created by jkatelaa on 9/30/16.
  */
trait SmtWrapper {

  def runSmtQuery(query : Seq[SmtCommand]) : String

}
