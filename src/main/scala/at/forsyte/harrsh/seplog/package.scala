package at.forsyte.harrsh

/**
  * Created by jkatelaa on 11/3/16.
  */
package object seplog {

  type VarNaming = Var => String

  type VarUnNaming = String => Var

  lazy val DefaultNaming : VarNaming = {
    case Var(0) => "null"
    case Var(i) if i > 0 => Var.FreeVarString + i
    case Var(i) => Var.BoundVarString + (-i)
  }

  def mkNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarNaming = {
    val freeVarNaming = freeVars.zipWithIndex map (p => (Var(p._2+1),p._1))
    val boundVarNaming = boundVars.zipWithIndex map (p => (Var(-(p._2+1)),p._1))
    Map.empty[Var,String] ++ freeVarNaming ++ boundVarNaming
  }

  def mkUnNaming(freeVars : Seq[String], boundVars : Seq[String]) : VarUnNaming = {
    val freeVarNaming = freeVars.zipWithIndex map (p => (p._1,Var(p._2+1)))
    val boundVarNaming = boundVars.zipWithIndex map (p => (p._1,Var(-(p._2+1))))
    Map.empty[String, Var] ++ freeVarNaming ++ boundVarNaming
  }

}
