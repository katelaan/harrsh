package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog._

/**
  * Created by jens on 3/14/17.
  */
case class PointsTo(from : PtrExpr, to : Seq[PtrExpr]) extends SepLogAtom {
  
  override def isSpatial = true

  override def isPure = false

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this)))

  override def renameVars(f : Renaming) : PointsTo = PointsTo(from.renameVars(f), to map (_.renameVars(f)))

  override def getVars : Set[Var] = (from +: to).toSet[PtrExpr] flatMap (_.getVar)

  def fromAsVar : Var = from.getVarUnsafe
  // TODO Check if we actually need the first variant
  def toAsVar : Seq[Var] = to map (_.getVarUnsafe)
  def toAsVarOrZero : Seq[Var] = to map (_.getVarOrZero)

  override def toStringWithVarNames(names: VarNaming): String = from.toStringWithVarNames(names) + " \u21a6 " + (if (to.tail.isEmpty) to.head.toStringWithVarNames(names).toString else to.map(_.toStringWithVarNames(names)).mkString("(", ", ", ")"))
}

object PointsTo {

  def apply(from : Var, to : Var) : PointsTo = PointsTo(PtrExpr(from), Seq(PtrExpr(to)))

  def apply(from : Var, to : Seq[Var]) : PointsTo = PointsTo(PtrExpr(from), to map (PtrExpr(_)))

}