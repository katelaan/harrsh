package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.Predicate
import at.forsyte.harrsh.util.ToLatex._

import scala.collection.mutable

case class ContextPredCall(pred: Predicate, subst: Substitution) {

  override def toString: String = {
    val args = subst.toSeq map {
      set => if (set.size == 1) set.head else set.mkString("{", " ", "}")
    }
    s"${pred.head}(${args.mkString(", ")})"
  }

  def update(f: SubstitutionUpdate): ContextPredCall = copy(subst = subst.update(f))

  def freeVarSeq: Seq[FreeVar] = pred.params

  def placeholders: Set[PlaceholderVar] = subst.placeholders

  lazy val rootParamSubst: Option[Set[Var]] = {
    pred.rootParamIndex map {
      ix => subst.toSeq(ix)
    }
  }

  def symbolicHeapLabel: String = {
    '$' + pred.defaultCall.toLatex.replaceAllLiterally("α", """\alpha""") + '$'
  }
}

object ContextPredCall {

  def toPlaceholderNormalForm(orderedNodeLabels: Seq[ContextPredCall]): SubstitutionUpdate = {
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