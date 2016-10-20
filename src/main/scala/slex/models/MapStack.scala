package slex.models

import slex.Sorts.Location
import slex.seplog._
import slex.seplog.indexed._

/**
  * Created by jkatelaa on 10/3/16.
  */
case class MapStack(vals : Map[String, Location]) extends Stack {

  override def apply(ptr : PtrExpr) : Location = ptr match {
    case NullPtr() => vals("null") // FIXME [NULL-TREATMENT] Only because we're treating null as an ordinary constant, might change in the future
    case PtrVar(id) => vals(id)
  }

  override def apply(ix : IntExpr) : Int = ix match {
    case IntVar(id) => vals(id)
    case _ => throw new Exception("Cannot look up stack value of non-variable expression " + ix)
  }

  override def toString = vals map { case (k,v) => k + " => " + v } mkString ("Stack(", ", ", ")")

}
