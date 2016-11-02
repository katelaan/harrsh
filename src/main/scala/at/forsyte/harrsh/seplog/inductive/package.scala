package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.main.FV
import at.forsyte.harrsh.main.FV._

import scala.language.implicitConversions

/**
  * Created by jkatelaa on 9/30/16.
  */
package object inductive {

//  implicit def stringToFV(s : String) : FV = qvar(s)

  type VarNaming = FV => String

  type VarUnNaming = String => FV

  lazy val DefaultNaming : VarNaming = _ match {
    case 0 => "null"
    case i if i > 0 => FV.FreeVarString + i
    case i => FV.BoundVarString + (-i)
  }

  def mkNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarNaming = {
    val freeVarNaming = freeVars.zipWithIndex map (p => (p._2+1,p._1))
    val boundVarNaming = boundVars.zipWithIndex map (p => (-(p._2+1),p._1))
    Map.empty[FV,String] ++ freeVarNaming ++ boundVarNaming
  }

  def mkUnNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarUnNaming = {
    // TODO Some code duplication here
    val freeVarNaming = freeVars.zipWithIndex map (p => (p._1,p._2+1))
    val boundVarNaming = boundVars.zipWithIndex map (p => (p._1,-(p._2+1)))
    Map.empty[String, FV] ++ freeVarNaming ++ boundVarNaming
  }

  def call(name : String, args : FV*) : PredCall = PredCall(name, args map PtrExpr.fromFV)

  def ptr(from : FV, to : FV*) : PointsTo = PointsTo(PtrVar(from), to map PtrExpr.fromFV)

  def nil : FV = fv(0)

  def qv(i : Int) : FV = -i

  //private var GlobalVarNames : Map[String,FV] = Map.empty

//  def qvar(s : String) : FV = {
//    if (GlobalVarNames.isDefinedAt(s)) GlobalVarNames(s) else {
//      GlobalVarNames = GlobalVarNames + (s -> (GlobalVarNames.values.min - 1))
//      GlobalVarNames(s)
//    }
//  }

  def emp : SpatialAtom = Emp()

  def ptreq(left : FV, right : FV) : PureAtom = PtrEq(PtrExpr.fromFV(left), PtrExpr.fromFV(right))

  def ptrneq(left : FV, right : FV) : PureAtom = PtrNEq(PtrExpr.fromFV(left), PtrExpr.fromFV(right))

}
