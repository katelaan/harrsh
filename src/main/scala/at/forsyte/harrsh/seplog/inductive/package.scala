package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.Var._

import scala.language.implicitConversions

/**
  * Created by jkatelaa on 9/30/16.
  */
package object inductive {
  type VarNaming = Var => String

  type VarUnNaming = String => Var

  lazy val DefaultNaming : VarNaming = _ match {
    case 0 => "null"
    case i if i > 0 => Var.FreeVarString + i
    case i => Var.BoundVarString + (-i)
  }

  def mkNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarNaming = {
    val freeVarNaming = freeVars.zipWithIndex map (p => (p._2+1,p._1))
    val boundVarNaming = boundVars.zipWithIndex map (p => (-(p._2+1),p._1))
    Map.empty[Var,String] ++ freeVarNaming ++ boundVarNaming
  }

  def mkUnNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarUnNaming = {
    val freeVarNaming = freeVars.zipWithIndex map (p => (p._1,p._2+1))
    val boundVarNaming = boundVars.zipWithIndex map (p => (p._1,-(p._2+1)))
    Map.empty[String, Var] ++ freeVarNaming ++ boundVarNaming
  }

  def call(name : String, args : Var*) : PredCall = PredCall(name, args map PtrExpr.fromFV)

  def ptr(from : Var, to : Var*) : PointsTo = PointsTo(PtrVar(from), to map PtrExpr.fromFV)

  def nil : Var = mkVar(0)

  def qv(i : Int) : Var = -i

  def ptreq(left : Var, right : Var) : PureAtom = PtrEq(PtrExpr.fromFV(left), PtrExpr.fromFV(right))

  def ptrneq(left : Var, right : Var) : PureAtom = PtrNEq(PtrExpr.fromFV(left), PtrExpr.fromFV(right))

}
