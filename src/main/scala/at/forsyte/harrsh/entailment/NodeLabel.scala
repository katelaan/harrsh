package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive.{Predicate, Rule}
import at.forsyte.harrsh.util.ToLatex._
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap.ops._

// FIXME: Nodes need an ID to make it possible for the same node to occur multiple times in the same tree. (Granted, a strange corner case, but we can't avoid that, can we?)

sealed trait NodeLabel {

  val subst: Substitution

  def isAbstractLeaf: Boolean = this match {
    case _:RuleNodeLabel => false
    case _:AbstractLeafNodeLabel => true
  }

  def symbolicHeapLabel: String = this match {
    case RuleNodeLabel(rule, _) => '$' + rule.body.toLatex(rule.naming).replaceAllLiterally("α", """\alpha""") + '$'
    case AbstractLeafNodeLabel(pred, _) => '$' + pred.defaultCall.toLatex.replaceAllLiterally("α", """\alpha""") + '$'
  }

  def freeVarSeq: Seq[FreeVar] = this match {
    case RuleNodeLabel(rule, _) => rule.body.freeVars
    case AbstractLeafNodeLabel(pred, _) => pred.params
  }

  def placeholders: Set[PlaceholderVar] = subst.placeholders

  def update(f: FreeVar => Set[FreeVar]): NodeLabel = this match {
    case n: RuleNodeLabel => n.copy(subst = n.subst.update(f))
    case n: AbstractLeafNodeLabel => n.copy(subst = n.subst.update(f))
  }
}

case class RuleNodeLabel(rule: Rule, override val subst: Substitution) extends NodeLabel {
  assert(subst.toMap.keySet == rule.body.freeVars.toSet)
}

case class AbstractLeafNodeLabel(pred: Predicate, override val subst: Substitution) extends NodeLabel {
  assert(subst.toMap.keySet == pred.defaultCall.freeVars.toSet)
}
