package slex.slsyntax

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait SpatialAtom extends SepLogFormula {

  def isSpatial = true

  def isPure = false

  def isSymbolicHeap = true

  def toSymbolicHeap = Some(SymbolicHeap(Seq(), Seq(this), Seq()))

}

case class Emp() extends SpatialAtom {
  override def toString = "emp"
}

case class PointsTo(from : PtrExpr, to : PtrExpr) extends SpatialAtom {
  override def toString = from + " \u2192 " + to
}

case class LSeg(from : PtrExpr, to : PtrExpr) extends SpatialAtom {
  override def toString = "lseg(" + from + ", " + to + ")"
}

case class IxLSeg(from : PtrExpr, to : PtrExpr, lngth : IntExpr) extends SpatialAtom {
  override def toString = "lseg(" + from + ", " + to + ", " +  lngth + ")"
}
