package at.forsyte.harrsh.smtinteraction

import at.forsyte.harrsh.smtsyntax.SmtCommand

/**
  * Created by jkatelaa on 10/5/16.
  */
case class SmtError(cmds : Seq[SmtCommand]) extends Throwable
