package slex.smtinteraction

import slex.smtsyntax.SmtCommand

/**
  * Created by jkatelaa on 10/5/16.
  */
case class SmtError(cmds : Seq[SmtCommand]) extends Throwable
