package at.forsyte.harrsh

/**
  * Created by jkatelaa on 11/3/16.
  */
package object seplog {

  type VarNaming = Var => String

  type VarUnNaming = String => Var

  lazy val DefaultNaming : VarNaming = v => v.toString

  def mkNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarNaming = {
    val freeVarNaming = freeVars map (v => (FreeVar(v),v))
    val boundVarNaming = boundVars.zipWithIndex map (p => (BoundVar(p._2+1),p._1))
    Map.empty[Var,String] ++ freeVarNaming ++ boundVarNaming ++ Map(NullConst -> NullConst.toString)
  }

  def mkUnNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarUnNaming = {
    val freeVarNaming = freeVars map (v => (v,FreeVar(v)))
    val boundVarNaming = boundVars.zipWithIndex map (p => (p._1,BoundVar(p._2+1)))
    Map.empty[String, Var] ++ freeVarNaming ++ boundVarNaming ++ Map(NullConst.toString -> NullConst)
  }

}
