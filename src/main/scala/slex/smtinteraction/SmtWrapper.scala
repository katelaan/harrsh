package slex.smtinteraction

import slex.models.Stack
import slex.smtsyntax.SmtCommand

/**
  * Created by jkatelaa on 9/30/16.
  */
trait SmtWrapper {

  def restart() : Unit

  def close(): Unit

  def addCommands(query : Seq[SmtCommand]) : Unit

  def checkSat() : SatStatus

  def computeModel() : Option[Stack]

}

object SmtWrapper {

  /**
    * Apply wrapper to f, then close the wrapper.
    */
  def withWrapper(wrapper : SmtWrapper)(f : SmtWrapper => Unit) = {
    f(wrapper)
    wrapper.close()
  }

  def withZ3 = withWrapper(new SimpleZ3Wrapper())_

}