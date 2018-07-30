package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.Var.Naming

/**
  * Created by jens on 11/2/16.
  */
case class Rule(head : String, qvarNames : Seq[String], body : SymbolicHeap) {

  private lazy val freeVarNames = body.freeVars.map(_.toString)

  lazy val naming : Naming = Naming.mkNaming(freeVarNames, qvarNames)

  override def toString = head + freeVarNames.mkString("(",", ", ")" + " <= " + body.toStringWithVarNames(naming))

  def isBaseRule: Boolean = body.isReduced

  def isRecRule: Boolean = body.nonReduced

}

object Rule {

  def fromTuple(tuple : (String, Seq[String], SymbolicHeap)) = Rule(tuple._1, tuple._2, tuple._3)

}
