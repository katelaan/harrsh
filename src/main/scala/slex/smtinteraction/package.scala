package slex

import slex.models.Stack

/**
  * Created by jkatelaa on 10/4/16.
  */
package object smtinteraction {

  type SmtOutput = (SatStatus, Option[Stack])

}
