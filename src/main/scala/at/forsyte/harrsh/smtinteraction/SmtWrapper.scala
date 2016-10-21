package at.forsyte.harrsh.smtinteraction

import at.forsyte.harrsh.models.Stack
import at.forsyte.harrsh.smtsyntax.SmtCommand

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