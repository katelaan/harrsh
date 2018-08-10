package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive.{Predicate, Rule}
import at.forsyte.harrsh.util.ToLatex._
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap.ops._

// FIXME: Nodes need an ID to make it possible for the same node to occur multiple times in the same tree. (Granted, a strange corner case, but we can't avoid that, can we?)

sealed trait UTNode {

  val labels: Labeling

  def isAbstractLeaf: Boolean = this match {
    case _:RuleNode => false
    case _:AbstractLeafNode => true
  }

  def symbolicHeapLabel: String = this match {
    case RuleNode(rule, _) => '$' + rule.body.toLatex(rule.naming).replaceAllLiterally("α", """\alpha""") + '$'
    case AbstractLeafNode(pred, _) => '$' + pred.defaultCall.toLatex.replaceAllLiterally("α", """\alpha""") + '$'
  }

  def freeVarSeq: Seq[FreeVar] = this match {
    case RuleNode(rule, _) => rule.body.freeVars
    case AbstractLeafNode(pred, _) => pred.params
  }
}

case class RuleNode(rule: Rule, override val labels: Labeling) extends UTNode {
  assert(labels.toMap.keySet == rule.body.freeVars.toSet)
}

case class AbstractLeafNode(pred: Predicate, override val labels: Labeling) extends UTNode {
  assert(labels.toMap.keySet == pred.defaultCall.freeVars.toSet)
}
