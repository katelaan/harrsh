package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RuleBody}
import at.forsyte.harrsh.util.ToLatex._
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap.ops._

// FIXME: Nodes need an ID to make it possible for the same node to occur multiple times in the same tree. (Granted, a strange corner case, but we can't avoid that, can we?)

sealed trait NodeLabel {

  val pred: Predicate
  val subst: Substitution

  assert(subst.size == pred.arity)

  def isAbstractLeaf: Boolean = this match {
    case _:RuleNodeLabel => false
    case _:AbstractLeafNodeLabel => true
  }

  def symbolicHeapLabel: String = this match {
    case RuleNodeLabel(_, rule, _) => '$' + rule.body.toLatex(rule.naming).replaceAllLiterally("α", """\alpha""") + '$'
    case AbstractLeafNodeLabel(pred, _) => '$' + pred.defaultCall.toLatex.replaceAllLiterally("α", """\alpha""") + '$'
  }

  def freeVarSeq: Seq[FreeVar] = pred.params

  def placeholders: Set[PlaceholderVar] = subst.placeholders

  def update(f: SubstitutionUpdate): NodeLabel

  lazy val rootVarSubst: Set[Var] = {
    val rootVarIx = freeVarSeq.indexOf(pred.rootParam.get)
    subst.toSeq(rootVarIx)
  }
}

// FIXME: Add predicate parameter to be able to associate the rule with the correct predicate
case class RuleNodeLabel(override val pred: Predicate, rule: RuleBody, override val subst: Substitution) extends NodeLabel {
  override def toString: String = s"rule($rule, $subst)"

  override def update(f: SubstitutionUpdate): RuleNodeLabel = copy(subst = subst.update(f))
}

case class AbstractLeafNodeLabel(override val pred: Predicate, override val subst: Substitution) extends NodeLabel {

  override def toString: String = s"leaf(${pred.head}, $subst)"

  override def update(f: SubstitutionUpdate): AbstractLeafNodeLabel = copy(subst = subst.update(f))
}
