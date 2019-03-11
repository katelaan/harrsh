package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var.Naming

/**
  * Created by jens on 11/2/16.
  */
case class RuleBody(qvarNames : Seq[String], body : SymbolicHeap) {

  lazy val freeVarNames: Seq[String] = body.freeVars.map(_.toString)
  lazy val naming: Naming = Naming.mkNaming(freeVarNames, qvarNames)

  lazy val isEmptyBaseRule: Boolean = body.predCalls.isEmpty && !body.hasPointer

  override def toString: String = body.toStringWithVarNames(naming)

  def isBaseRule: Boolean = body.isReduced

  def isRecRule: Boolean = body.nonReduced

  def hasPointer: Boolean = body.hasPointer

  def hasCallsButNoPointers: Boolean = body.predCalls.nonEmpty && !body.hasPointer

  def satisfiesGeneralizedProgress(rootOfPred: Option[Var]): Boolean = {
    body.pointers.length match {
      case 0 =>
        // Generalized progress: We allow rules without allocation provided they contain no (--> emp) or at least two recursive calls.
        // This way, we can deal with rules such as SLSLList <= SLSLList(x1,y2) * SLSLList(y2,x2);
        (body.pure.isEmpty && body.predCalls.size > 1) || body.predCalls.isEmpty
      case 1 =>
        // Progress in the strict sense:
        // There is exactly one pointer, originating in the root of the predicate (if given)
        rootOfPred match {
          case Some(root) => body.pointers.head.from == root
          case None => true
        }
      case _ =>
        false
    }
  }

}
