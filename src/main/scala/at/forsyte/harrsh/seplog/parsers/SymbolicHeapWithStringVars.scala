package at.forsyte.harrsh.seplog.parsers

import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._

/**
  * Exact copy of the symbolic heap class hierarchy, but with strings rather than integers as variable identifiers
  */
case class StringSymbolicHeap(pure : Seq[StringPureAtom], spatial : Seq[StringSpatialAtom]) {
  def replaceStringsByIds(naming: VarUnNaming): SymbolicHeap = SymbolicHeap(pure map (_.replaceStringsByIds(naming)), spatial map (_.replaceStringsByIds(naming)))

  def getVars : Set[String] = Set.empty ++ pure.flatMap(_.getVars) ++ spatial.flatMap(_.getVars)
}

trait StringSepLogAtom {
  def getVars : Set[String]

  def replaceStringsByIds(naming: VarUnNaming) : SepLogAtom
}

sealed trait StringPureAtom extends StringSepLogAtom {
  override def getVars: Set[String] = this match {
    case StringTrue() => Set.empty
    case StringPtrEq(l, r) => l.getVars union r.getVars
    case StringPtrNEq(l, r) => l.getVars union r.getVars
  }

  override def replaceStringsByIds(naming: VarUnNaming) : PureAtom = this match {
    case StringTrue() => True()
    case StringPtrEq(l, r) => PtrEq(l.replaceStringsByIds(naming), r.replaceStringsByIds(naming))
    case StringPtrNEq(l, r) => PtrNEq(l.replaceStringsByIds(naming), r.replaceStringsByIds(naming))
  }
}

case class StringTrue() extends StringPureAtom

case class StringPtrEq(l : StringPtrExpr, r : StringPtrExpr) extends StringPureAtom

case class StringPtrNEq(l : StringPtrExpr, r : StringPtrExpr) extends StringPureAtom

sealed trait StringSpatialAtom extends StringSepLogAtom {
  override def getVars: Set[String] = this match {
    case StringEmp() => Set.empty
    case StringPointsTo(from, to) => from.getVars ++ to.flatMap(_.getVars)
    case StringPredCall(name, args) => Set.empty ++ args.flatMap(_.getVars)
  }

  override def replaceStringsByIds(naming: VarUnNaming) : SpatialAtom = this match {
    case StringEmp() => Emp()
    case StringPointsTo(from, to) => PointsTo(from.replaceStringsByIds(naming), to.map(_.replaceStringsByIds(naming)))
    case StringPredCall(name, args) => PredCall(name, args.map(_.replaceStringsByIds(naming)))
  }
}

case class StringEmp() extends StringSpatialAtom

case class StringPointsTo(from : StringPtrExpr, to : Seq[StringPtrExpr]) extends StringSpatialAtom

case class StringPredCall(name : String, args : Seq[StringPtrExpr]) extends StringSpatialAtom

sealed trait StringPtrExpr {
  def getVars: Set[String] = this match {
    case StringNullPtr() => Set.empty
    case StringPtrVar(id) => Set(id)
  }

  def replaceStringsByIds(naming: VarUnNaming) : PtrExpr = this match {
    case StringNullPtr() => NullPtr()
    case StringPtrVar(id) => PtrVar(naming(id))
  }
}

case class StringNullPtr() extends StringPtrExpr

case class StringPtrVar(id : String) extends StringPtrExpr