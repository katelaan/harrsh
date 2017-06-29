package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.{Var, PtrExpr, PtrVar}
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 5/19/17.
  */
trait AtomConstructorFunctions {

  def call(name : String, args : Var*) : PredCall = PredCall(name, args map (PtrExpr(_)))

  def ptr(from : Var, to : Var*) : PointsTo = PointsTo(from, to)

  def ptreq(left : Var, right : Var) : PureAtom = PtrEq(left, right)

  def ptrneq(left : Var, right : Var) : PureAtom = PtrNEq(left, right)

}
