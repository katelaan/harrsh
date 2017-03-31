package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{Var, VarNaming, mkNaming}

/**
  * Created by jens on 11/2/16.
  */
case class Rule(head : String, freeVars : Seq[String], qvars : Seq[String], body : SymbolicHeap) {

  lazy val naming : VarNaming = mkNaming(freeVars, qvars)

  override def toString = head + freeVars.mkString("(",", ", ")" + " <= " + body.toStringWithVarNames(naming))

}

object Rule {

  def fromTuple(tuple : (String, Seq[String], SymbolicHeap)) = Rule(tuple._1, (1 to tuple._3.numFV) map Var.toDefaultString, tuple._2, tuple._3)

}
