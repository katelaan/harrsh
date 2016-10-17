package slex.seplog

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait SpatialAtom extends SepLogFormula {

  override def isSpatial = true

  override def isPure = false

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(), Seq(this), Seq()))

  override def renameVars(f : Renaming) : SpatialAtom = this match {
    case e : Emp => e
    case PointsTo(from, to) => PointsTo(from.renameVars(f), to.renameVars(f))
    case IxLSeg(from, to, lngth) => IxLSeg(from.renameVars(f), to.renameVars(f), lngth.renameVars(f))
    case call : PredCall => call.copy(args = call.args map (_.renameVars(f)))
  }

  def isInductiveCall: Boolean = this match {
    case _ : PredCall => true
    case _ => false
  }

  def getPredicateName: Option[String] = this match {
    case p : PredCall => Some(p.name)
    case _ => None
  }

}

case class Emp() extends SpatialAtom {
  override def toString = "emp"
}

case class PointsTo(from : PtrExpr, to : PtrExpr) extends SpatialAtom {
  override def toString = from + " \u21a6 " + to
}

/**
  * Length-indexed acyclic list segment
  * @param from
  * @param to
  * @param lngth
  */
case class IxLSeg(from : PtrExpr, to : PtrExpr, lngth : IntExpr) extends SpatialAtom {
  override def toString = "lseg(" + from + ", " + to + ", " +  lngth + ")"
}

/**
  * Inductive spatial predicate, whose semantics is given by a SID
  * @param name Name of the predicate
  * @param args Nonempty sequence of arguments
  */
case class PredCall(name : String, args : Seq[PtrExpr]) extends SpatialAtom {
  override def toString = name + "(" + args.mkString(",") + ")"
}
