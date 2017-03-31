package at.forsyte.harrsh.seplog

/**
  * Created by jkatelaa on 3/31/17.
  */
trait ToStringWithVarnames {

  override final def toString = toStringWithVarNames(DefaultNaming)

  def toStringWithVarNames(names: VarNaming): String

}
