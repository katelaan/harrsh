package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}
import PlaceholderVar._

import scala.util.Try

case class PlaceholderVar(index: Int) {

  def toFreeVar : FreeVar = FreeVar(placeholderPrefix + index)

}

object PlaceholderVar {

  val placeholderPrefix = "?"

  def fromVar(v: FreeVar) : Option[PlaceholderVar] = {
    if (v.name startsWith placeholderPrefix) {
      Try { Integer.parseInt(v.name.drop(placeholderPrefix.length)) }.toOption.map(PlaceholderVar(_))
    } else {
      None
    }
  }

  def isPlaceholder(v : FreeVar): Boolean = fromVar(v).nonEmpty

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

  def placeholderClashAvoidanceUpdate(ut: UnfoldingTree) : FreeVar => Set[FreeVar] = {
    val maxPv = max(ut.placeholders)
    val shiftBy = maxPv.index
    fv => fromVar(fv) match {
      case Some(PlaceholderVar(value)) => Set(PlaceholderVar(value + shiftBy).toFreeVar)
        case None => Set(fv)
    }
  }

}
