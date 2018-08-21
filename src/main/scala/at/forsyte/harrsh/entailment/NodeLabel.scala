package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RuleBody}
import at.forsyte.harrsh.util.ToLatex._

import scala.collection.mutable

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

case class RuleNodeLabel(override val pred: Predicate, rule: RuleBody, override val subst: Substitution) extends NodeLabel {
  override def toString: String = s"${pred.head}.rule($rule, $subst)"

  override def update(f: SubstitutionUpdate): RuleNodeLabel = copy(subst = subst.update(f))
}

case class AbstractLeafNodeLabel(override val pred: Predicate, override val subst: Substitution) extends NodeLabel {

  override def toString: String = s"leaf(${pred.head}, $subst)"

  override def update(f: SubstitutionUpdate): AbstractLeafNodeLabel = copy(subst = subst.update(f))
}

object NodeLabel {

  def labelsToPlaceholderNormalForm(orderedNodeLabels: Seq[NodeLabel]): SubstitutionUpdate = {
    val found = mutable.Set.empty[PlaceholderVar]
    val order = new mutable.ListBuffer[PlaceholderVar]()
    for {
      nodeLabel <- orderedNodeLabels
      vs <- nodeLabel.subst.toSeq
      v <- vs
      ph <- PlaceholderVar.fromVar(v)
      if !found.contains(ph)
    } {
      order.append(ph)
      found.add(ph)
    }
    val renameFrom = order map (_.toFreeVar)
    val renameTo = (1 to order.size) map (PlaceholderVar(_).toFreeVar)
    SubstitutionUpdate.fromPairs(renameFrom.zip(renameTo))
  }

}