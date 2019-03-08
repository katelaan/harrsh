package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RichSid}
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

  def hasNullInRootPosition(sid: RichSid): Boolean = {
    sid.rootParamIndex.get(pred.head) match {
      case Some(ix) => subst.toSeq(ix).contains(NullConst)
      case None => false
    }
  }

  def rootParamSubst(sid: RichSid): Option[Set[Var]] = {
    sid.rootParamIndex.get(pred.head) map {
      ix => subst.toSeq(ix)
    }
  }

  def symbolicHeapLabel: String = {
    '$' + pred.defaultCall.toLatex.replaceAllLiterally("α", """\alpha""") + '$'
  }

  /**
    * Order that uses lexicographic order on predicate name and, if that matches, uses lexicographic order on sequences
    * of non-placeholder vars stored in the substitution.
    *
    * This way, the ordering does not depend on the (arbitrary) names of the placeholder vars.
    */
  def lessThan(other: ContextPredCall): Boolean = {
    if (pred.head != other.pred.head) {
      pred.head < other.pred.head
    } else {
      val (thisNonPh, otherNonPh) = (subst.orderedNonPlaceholders, other.subst.orderedNonPlaceholders)
      val (thisSize, otherSize) = (thisNonPh.size, otherNonPh.size)
      if (thisSize != otherSize) {
        thisSize < otherSize
      }
      else {
        (thisNonPh zip otherNonPh).find(p => p._1 != p._2) match {
          case None => false
          case Some((thisVar, otherVar)) => thisVar < otherVar
        }
      }
    }
  }
}

object ContextPredCall extends HarrshLogging {

  def placeholderNormalFormUpdater(orderedNodeLabels: Seq[ContextPredCall]): SubstitutionUpdate = {
    logger.trace("Will order placeholders in order of occurrence in " + orderedNodeLabels)
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
    val pairs = renameFrom.zip(renameTo)
    logger.trace("Will establish placeholder normalform via update " + pairs)
    SubstitutionUpdate.fromPairs(pairs)
  }

}