package at.forsyte.harrsh.seplog.indexed

import at.forsyte.harrsh.seplog.{PtrExpr, Renaming}

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait IndexedSpatialAtom extends IndexedSepLogFormula {

  override def isSpatial = true

  override def isPure = false

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(IndexedSymbolicHeap(Seq(), Seq(this), Seq()))

  override def renameVars(f : Renaming) : IndexedSpatialAtom = this match {
    case e : Emp => e
    case PointsTo(from, to) => PointsTo(from.renameVars(f), to map (_.renameVars(f)))
    case IxLSeg(from, to, lngth) => IxLSeg(from.renameVars(f), to.renameVars(f), lngth.renameVars(f))
  }

  def getVars : Set[String] = this match {
    case Emp() => Set()
    case PointsTo(from, to) => (from +: to).toSet[PtrExpr] flatMap (_.getVar)
    case IxLSeg(from, to, lngth) => Set(from.getVar, to.getVar, lngth.getVars).flatten
  }

}

case class Emp() extends IndexedSpatialAtom {
  override def toString = "emp"
}

case class PointsTo(from : PtrExpr, to : Seq[PtrExpr]) extends IndexedSpatialAtom {
  override def toString = from + " \u21a6 " + (if (to.tail.isEmpty) to.head.toString else to.mkString("(", ", ", ")"))
}

/**
  * Length-indexed acyclic list segment
  * @param from Source pointer
  * @param to Target pointer
  * @param lngth Length of the list segment
  */
case class IxLSeg(from : PtrExpr, to : PtrExpr, lngth : IntExpr) extends IndexedSpatialAtom {
  override def toString = "lseg(" + from + ", " + to + ", " +  lngth + ")"
}