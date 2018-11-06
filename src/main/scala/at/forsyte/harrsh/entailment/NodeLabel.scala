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
    case _:PredicateNodeLabel => true
  }

  def symbolicHeapLabel: String = this match {
    case RuleNodeLabel(_, rule, _) => '$' + rule.body.toLatex(rule.naming).replaceAllLiterally("α", """\alpha""") + '$'
    case PredicateNodeLabel(pred, _) => '$' + pred.defaultCall.toLatex.replaceAllLiterally("α", """\alpha""") + '$'
  }

  def freeVarSeq: Seq[FreeVar] = pred.params

  def placeholders: Set[PlaceholderVar] = subst.placeholders

  def update(f: SubstitutionUpdate): NodeLabel

  lazy val rootParamSubst: Option[Set[Var]] = {
    pred.rootParamIndex map {
      ix => subst.toSeq(ix)
    }
  }

  def varUsage(freeVar: FreeVar): VarUsage

  def toPredicateNodeLabel: PredicateNodeLabel = this match {
    case l: PredicateNodeLabel => l
    case _ => PredicateNodeLabel(pred, subst)
  }
}

case class RuleNodeLabel(override val pred: Predicate, rule: RuleBody, override val subst: Substitution) extends NodeLabel {
  override def toString: String = s"${pred.head}.rule($rule, $subst)"

  override def update(f: SubstitutionUpdate): RuleNodeLabel = copy(subst = subst.update(f))

  override def varUsage(freeVar: FreeVar): VarUsage = {
    assert(rule.body.pointers.size == 1)
    val ptr = rule.body.pointers.head
    if (ptr.from == freeVar) VarUsage.Allocated
    else if (ptr.to.contains(freeVar)) VarUsage.Referenced
    else VarUsage.Unused
  }
}

case class PredicateNodeLabel(override val pred: Predicate, override val subst: Substitution) extends NodeLabel {

  override def toString: String = s"(${pred.head}, $subst)"

  override def update(f: SubstitutionUpdate): PredicateNodeLabel = copy(subst = subst.update(f))

  override def varUsage(freeVar: FreeVar): VarUsage = {
    // The abstract leaves themselves don't use the variables in any way
    VarUsage.Unused
  }
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