package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{SepLogFormula,Var}

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SepLogAtom extends SepLogFormula {
  def getNonNullVars : Set[Var]
}
