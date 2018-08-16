package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{SepLogSyntax,Var}

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SepLogAtom extends SepLogSyntax {
  def getNonNullVars : Set[Var]
}
