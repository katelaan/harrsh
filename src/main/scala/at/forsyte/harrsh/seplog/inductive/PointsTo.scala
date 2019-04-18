package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{Renaming, Var}
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

/**
  * Created by jens on 3/14/17.
  */
case class PointsTo(from : Var, to : Seq[Var]) extends SepLogAtom {

  lazy val args: Seq[Var] = from +: to

  override def isSpatial = true

  override def isPure = false

  override def toSymbolicHeap = SymbolicHeap(Seq.empty, Seq(this), Seq.empty, Var.freeNonNullVars(getNonNullVars).toSeq)

  override def renameVars(f : Renaming) : PointsTo = PointsTo(from.rename(f), to map (_.rename(f)))

  override def getVars : Set[Var] = (from +: to).toSet

  override def toStringWithVarNames(names: Naming): String = names(from) + " \u21a6 " + (if (to.tail.isEmpty) names(to.head).toString else to.map(names).mkString("(", ", ", ")"))
}

object PointsTo {

  def apply(from : Var, to : Var) : PointsTo = PointsTo(from, Seq(to))

  implicit val pointsToToLatex: ToLatex[PointsTo] = (pointsTo: PointsTo, naming: Naming) => {
    val argString = pointsTo.to.map(_.toLatex(naming)).mkString(",")
    val parensString = if (pointsTo.to.size > 1) s"($argString)" else argString
    s"${pointsTo.from.toLatex(naming)} \\rightarrow $parensString"
  }


}