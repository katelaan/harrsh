package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{NullConst, SepLogSyntax, Var}

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SepLogAtom extends SepLogSyntax {
  def getVars : Set[Var]

  final def getNonNullVars : Set[Var] = getVars - NullConst
}
