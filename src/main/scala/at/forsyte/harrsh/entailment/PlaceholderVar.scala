package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{FreeVar, Var}
import PlaceholderVar._

import scala.util.Try

case class PlaceholderVar(index: Int) {

  def toFreeVar : FreeVar = FreeVar(placeholderPrefix + index)

}

object PlaceholderVar {

  val placeholderPrefix = "?"

  def fromVar(v: Var) : Option[PlaceholderVar] = v match {
    case FreeVar(name) =>
      if (name startsWith placeholderPrefix) {
        Try { Integer.parseInt(name.drop(placeholderPrefix.length)) }.toOption.map(PlaceholderVar(_))
      } else {
        None
      }
    case _ => None
  }

  def isPlaceholder(v : Var): Boolean = fromVar(v).nonEmpty

  def max(pvs: Iterable[PlaceholderVar]): PlaceholderVar = try {
    pvs.maxBy(_.index)
  } catch {
    case e: UnsupportedOperationException => PlaceholderVar(0)
  }

  def maxIndex(pvs: Iterable[PlaceholderVar]): Int = max(pvs).index

  def min(pvs: Iterable[PlaceholderVar]): PlaceholderVar = try {
    pvs.minBy(_.index)
  } catch {
    case e: UnsupportedOperationException => PlaceholderVar(0)
  }

  def containsNoRedundantPlaceholder(vs: Set[Var]): Boolean = {
    // There's at most one placeholder in the set and if there's one in the set it's the only element
    val numPhs = vs.count(PlaceholderVar.isPlaceholder)
    numPhs <= 1 && (numPhs == 0 || vs.size == 1)
  }

  def noGapsInPlaceholders(phs: Iterable[PlaceholderVar]): Boolean = {
    phs.isEmpty || phs.map(_.index).max == phs.size
  }
  
  def placeholderClashAvoidanceUpdate(ut: UnfoldingTree) : SubstitutionUpdate = {
    placeholderClashAvoidanceUpdate(ut.placeholders)
  }

  def placeholderClashAvoidanceUpdate(phs: Set[PlaceholderVar]) : SubstitutionUpdate = {
    val maxPv = max(phs)
    val shiftBy = maxPv.index
    fv => fromVar(fv) match {
      case Some(PlaceholderVar(value)) => Set(PlaceholderVar(value + shiftBy).toFreeVar)
      case None => Set(fv)
    }
  }

}
