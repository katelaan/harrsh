package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{PtrExpr, Renaming, Var}

/**
  * Created by jkatelaa on 10/3/16.
  */
//trait SpatialAtom extends SepLogAtom {
//
//  override def isSpatial = true
//
//  override def isPure = false
//
//  override def isSymbolicHeap = true
//
//  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this)))
//}


sealed trait SpatialAtom extends SepLogAtom {

  override def isSpatial = true

  override def isPure = false

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this)))

  override def renameVars(f : Renaming) : SpatialAtom = this match {
    case e : Emp => e
    case PointsTo(from, to) => PointsTo(from.renameVars(f), to map (_.renameVars(f)))
  }

  def getVars : Set[Var] = this match {
    case Emp() => Set()
    case PointsTo(from, to) => (from +: to).toSet[PtrExpr] flatMap (_.getVar)
  }

}

case class Emp() extends SpatialAtom {
  override def toStringWithVarNames(names: VarNaming) = "emp"
}

case class PointsTo(from : PtrExpr, to : Seq[PtrExpr]) extends SpatialAtom {

  def fromAsVar : Var = from.getVarUnsafe
  def toAsVar : Seq[Var] = to map (_.getVarUnsafe)

  override def toStringWithVarNames(names: VarNaming): String = from.toStringWithVarNames(names) + " \u21a6 " + (if (to.tail.isEmpty) to.head.toStringWithVarNames(names).toString else to.map(_.toStringWithVarNames(names)).mkString("(", ", ", ")"))
}

