package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.seplog.Var.UnNaming
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._

/**
  * Exact copy of the symbolic heap class hierarchy, but with strings rather than integers as variable identifiers
  */
case class StringSymbolicHeap(pure : Seq[StringPureAtom], spatial : Seq[StringSpatialAtom]) {

  def replaceStringsByIds(naming: UnNaming): SymbolicHeap = {
    val allUnnamedSpatial : Seq[SepLogAtom] = spatial map (_.replaceStringsByIds(naming)) filter (_.isDefined) map (_.get)
    val (predCalls, nonCalls) = allUnnamedSpatial.partition(_.isInstanceOf[PredCall])

    val unnamedPure = pure map (_.replaceStringsByIds(naming)) filter (_.isDefined) map (_.get)
    val atoms = AtomContainer(unnamedPure, nonCalls map (_.asInstanceOf[PointsTo]), predCalls map (_.asInstanceOf[PredCall]))
    SymbolicHeap(atoms, atoms.freeVarSeq)
  }

  def getVars : Set[String] = Set.empty ++ pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars)
}

trait StringSepLogAtom {
  def getVars : Set[String]

  def replaceStringsByIds(naming: UnNaming) : Option[SepLogAtom]
}

sealed trait StringPureAtom extends StringSepLogAtom {
  override def getVars: Set[String] = this match {
    case StringTrue => Set.empty
    case StringPtrEq(l, r) => l.getVars union r.getVars
    case StringPtrNEq(l, r) => l.getVars union r.getVars
  }

  override def replaceStringsByIds(naming: UnNaming) : Option[PureAtom] = this match {
    case StringTrue => None
    case StringPtrEq(l, r) => Some(l.replaceStringsByIds(naming) =:= r.replaceStringsByIds(naming))
    case StringPtrNEq(l, r) => Some(l.replaceStringsByIds(naming) =/= r.replaceStringsByIds(naming))
  }
}

case object StringTrue extends StringPureAtom

case class StringPtrEq(l : StringPtrExpr, r : StringPtrExpr) extends StringPureAtom

case class StringPtrNEq(l : StringPtrExpr, r : StringPtrExpr) extends StringPureAtom

sealed trait StringSpatialAtom extends StringSepLogAtom {
  override def getVars: Set[String] = this match {
    case StringEmp => Set.empty
    case StringPointsTo(from, to) => from.getVars ++ to.flatMap(_.getVars)
    case StringPredCall(name, args) => Set.empty ++ args.flatMap(_.getVars)
  }

  override def replaceStringsByIds(naming: UnNaming) : Option[SepLogAtom] = this match {
    case StringEmp => None
    case StringPointsTo(from, to) => Some(PointsTo(from.replaceStringsByIds(naming), to.map(_.replaceStringsByIds(naming))))
    case StringPredCall(name, args) => Some(PredCall(name, args.map(_.replaceStringsByIds(naming))))
  }
}

case object StringEmp extends StringSpatialAtom

case class StringPointsTo(from : StringPtrExpr, to : Seq[StringPtrExpr]) extends StringSpatialAtom

case class StringPredCall(name : String, args : Seq[StringPtrExpr]) extends StringSpatialAtom

sealed trait StringPtrExpr {
  def getVars: Set[String] = this match {
    case StringNullPtr() => Set.empty
    case StringPtrVar(id) => Set(id)
  }

  def replaceStringsByIds(naming: UnNaming) : Var = this match {
    case StringNullPtr() => NullConst
    case StringPtrVar(id) => naming(id)
  }
}

case class StringNullPtr() extends StringPtrExpr

case class StringPtrVar(id : String) extends StringPtrExpr