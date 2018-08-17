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

  def min(pvs: Iterable[PlaceholderVar]): PlaceholderVar = try {
    pvs.minBy(_.index)
  } catch {
    case e: UnsupportedOperationException => PlaceholderVar(0)
  }

  def placeholderClashAvoidanceUpdate(ut: UnfoldingTree) : SubstitutionUpdate = {
    val maxPv = max(ut.placeholders)
    val shiftBy = maxPv.index
    fv => fromVar(fv) match {
      case Some(PlaceholderVar(value)) => Set(PlaceholderVar(value + shiftBy).toFreeVar)
      case None => Set(fv)
    }
  }

}
