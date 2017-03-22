package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog._

/**
  * Created by jkatelaa on 10/20/16.
  */
trait SepLogAtom extends SepLogFormula {
  def getVars : Set[Var]
}
