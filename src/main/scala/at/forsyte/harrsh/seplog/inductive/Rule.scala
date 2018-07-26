package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{VarNaming, mkNaming}

/**
  * Created by jens on 11/2/16.
  */
case class Rule(head : String, qvarNames : Seq[String], body : SymbolicHeap) {

  private lazy val freeVarNames = body.freeVars.map(_.toString)

  lazy val naming : VarNaming = mkNaming(freeVarNames, qvarNames)

  override def toString = head + freeVarNames.mkString("(",", ", ")" + " <= " + body.toStringWithVarNames(naming))

}

object Rule {

  def fromTuple(tuple : (String, Seq[String], SymbolicHeap)) = Rule(tuple._1, tuple._2, tuple._3)

}
