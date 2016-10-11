package slex.smtinteraction

import slex.smtsyntax.SmtCommand

/**
  * Created by jkatelaa on 9/30/16.
  */
trait SmtWrapper {

  def restart() : Unit

  def close(): Unit

  def addCommands(query : Seq[SmtCommand]) : Unit

  def checkSat() : SmtOutput

  def getModel() : SmtOutput

}