package slex.algs

import slex.slsyntax.{IntExpr, PtrExpr}
import slex.Sorts._

/**
  * Created by jkatelaa on 10/3/16.
  */
trait Stack  {

  def apply(ptr : PtrExpr) : Location

  def apply(ix : IntExpr) : Int

}
