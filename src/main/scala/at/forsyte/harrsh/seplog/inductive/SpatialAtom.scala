package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.Var
import at.forsyte.harrsh.seplog.{PtrExpr, Renaming}

/**
  * Created by jkatelaa on 10/3/16.
  */
sealed trait SpatialAtom extends SepLogAtom {

  override def isSpatial = true

  override def isPure = false

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this)))

  override def renameVars(f : Renaming) : SpatialAtom = this match {
    case e : Emp => e
    case PointsTo(from, to) => PointsTo(from.renameVars(f), to map (_.renameVars(f)))
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

  def getVars : Set[Var] = this match {
    case Emp() => Set()
    case PointsTo(from, to) => (from +: to).toSet[PtrExpr] flatMap (_.getVar)
    case PredCall(name, args) => (args flatMap (_.getVar)).toSet
  }

}

case class Emp() extends SpatialAtom {
  override def toStringWithVarNames(names: VarNaming) = "emp"
}

case class PointsTo(from : PtrExpr, to : Seq[PtrExpr]) extends SpatialAtom {

  def fromAsVar : Var = from.getVarUnsafe
  def toAsVar : Seq[Var] = to map (_.getVarUnsafe)

  override def toStringWithVarNames(names: VarNaming): String = from + " \u21a6 " + (if (to.tail.isEmpty) to.head.toString else to.mkString("(", ", ", ")"))
}

/**
  * Inductive spatial predicate, whose semantics is given by a SID
  * @param name Name of the predicate
  * @param args Nonempty sequence of arguments
  */
case class PredCall(name : String, args : Seq[PtrExpr]) extends SpatialAtom {
  override def toStringWithVarNames(names: VarNaming) = name + "(" + args.map(_.toStringWithVarNames(names)).mkString(",") + ")"
}
