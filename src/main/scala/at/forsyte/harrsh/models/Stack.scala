package at.forsyte.harrsh.models

import at.forsyte.harrsh.Sorts._
import at.forsyte.harrsh.seplog.PtrExpr
import at.forsyte.harrsh.seplog.indexed.IntExpr

/**
  * Created by jkatelaa on 10/3/16.
  */
trait Stack  {

  def apply(ptr : PtrExpr) : Location

  def apply(ix : IntExpr) : Int

}
