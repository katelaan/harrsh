package slex.seplog

import scala.language.implicitConversions

/**
  * Created by jkatelaa on 9/30/16.
  */
package object indexed {

  implicit def stringToPtrExpr(s : String) : PtrExpr = PtrVar(s)

  implicit def stringToIntExpr(s : String) : IntExpr = IntVar(s)

  implicit def intToIntExpr(i : Int) : IntExpr = IntConst(i)

  def ptr(from : PtrExpr, to : PtrExpr*) : PointsTo = PointsTo(from, to)

  def nil : PtrExpr = NullPtr()

  def emp : IndexedSpatialAtom = Emp()

  def ptreq(left : PtrExpr, right : PtrExpr) : IndexedPureAtom = PtrEq(left, right)

  def ptrneq(left : PtrExpr, right : PtrExpr) : IndexedPureAtom = PtrNEq(left, right)

}
