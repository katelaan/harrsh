package slex.algs

import slex.Sorts.Location
import slex.slsyntax.{IntExpr, PtrExpr}

/**
  * Created by jkatelaa on 10/3/16.
  */
case class MapStack(ptrs : Map[PtrExpr, Location], ixs : Map[IntExpr, Int]) extends Stack {

  override def apply(ptr : PtrExpr) : Location = ptrs(ptr)

  override def apply(ix : IntExpr) : Int = ixs(ix)

}
