package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var.Naming

/**
  * Created by jens on 11/2/16.
  */
case class RuleBody(qvarNames : Seq[String], body : SymbolicHeap) {

  lazy val freeVarNames: Seq[String] = body.freeVars.map(_.toString)
  lazy val naming: Naming = Naming.mkNaming(freeVarNames, body.boundVars, qvarNames)

  lazy val isEmptyBaseRule: Boolean = body.predCalls.isEmpty && !body.hasPointer

  override def toString: String = body.toStringWithVarNames(naming)

  def isBaseRule: Boolean = body.isReduced

  def isRecRule: Boolean = body.nonReduced

  def hasPointer: Boolean = body.hasPointer

  def hasCallsButNoPointers: Boolean = body.predCalls.nonEmpty && !body.hasPointer

}
