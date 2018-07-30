package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.Var.Naming

/**
  * Created by jkatelaa on 3/31/17.
  */
trait ToStringWithVarnames {

  override final def toString = toStringWithVarNames(Naming.DefaultNaming)

  def toStringWithVarNames(names: Naming): String

}
