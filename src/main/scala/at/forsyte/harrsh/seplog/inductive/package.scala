package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.Var._

import scala.language.implicitConversions

/**
  * Created by jkatelaa on 9/30/16.
  */
package object inductive {
  def call(name : String, args : Var*) : PredCall = PredCall(name, args map PtrExpr.fromFV)

  def ptr(from : Var, to : Var*) : PointsTo = PointsTo(PtrVar(from), to map PtrExpr.fromFV)

  def nil : Var = 0

  def ptreq(left : Var, right : Var) : PureAtom = PtrEq(PtrExpr.fromFV(left), PtrExpr.fromFV(right))

  def ptrneq(left : Var, right : Var) : PureAtom = PtrNEq(PtrExpr.fromFV(left), PtrExpr.fromFV(right))

}
