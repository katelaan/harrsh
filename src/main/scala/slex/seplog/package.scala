package slex

/**
  * Created by jkatelaa on 9/30/16.
  */
package object seplog {

  implicit def stringToPtrExpr(s : String) : PtrExpr = PtrVar(s)

  implicit def stringToIntExpr(s : String) : IntExpr = IntVar(s)

  implicit def intToIntExpr(i : Int) : IntExpr = IntConst(i)

  def call(name : String, args : PtrExpr*) : PredCall = PredCall(name, args)

  def ptr(from : PtrExpr, to : PtrExpr*) : PointsTo = PointsTo(from, to)

  def nil : PtrExpr = NullPtr()

  def emp : SpatialAtom = Emp()

  def ptreq(left : PtrExpr, right : PtrExpr) : PureAtom = PtrEq(left, right)

  def ptrneq(left : PtrExpr, right : PtrExpr) : PureAtom = PtrNEq(left, right)

}
