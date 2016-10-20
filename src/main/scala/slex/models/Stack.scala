package slex.models

import slex.Sorts._
import slex.seplog.PtrExpr
import slex.seplog.indexed.IntExpr

/**
  * Created by jkatelaa on 10/3/16.
  */
trait Stack  {

  def apply(ptr : PtrExpr) : Location

  def apply(ix : IntExpr) : Int

}
