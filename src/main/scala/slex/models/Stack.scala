package slex.models

import slex.Sorts._
import slex.seplog.{IntExpr, PtrExpr}

/**
  * Created by jkatelaa on 10/3/16.
  */
trait Stack  {

  def apply(ptr : PtrExpr) : Location

  def apply(ix : IntExpr) : Int

}
