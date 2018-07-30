package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.{NullConst, Renaming, Var}

/**
  * Created by jkatelaa on 10/3/16.
  */
case class PureAtom(l: Var, r: Var, isEquality: Boolean) extends SepLogAtom with HarrshLogging {

  override def isSpatial = false

  override def isPure = true

  override def isSymbolicHeap = true

  override def toSymbolicHeap = Some(SymbolicHeap(Seq(this), Seq.empty, Seq.empty, Var.freeNonNullVars(Seq(l,r))))

  override def renameVars(f: Renaming): PureAtom = PureAtom(l.rename(f), r.rename(f), isEquality)

  override def getNonNullVars : Set[Var] = Set(l, r) - NullConst

  def getVarsWithNull : Set[Var] = Set(l, r)

  def comparesFree: Boolean = getNonNullVars.forall(_.isFree)

  def isPointerComparison = true

  def ordered : PureAtom = if (l < r) this else PureAtom(r, l, isEquality)

  override def toStringWithVarNames(names: Naming): String = {
    val op = if (isEquality) " \u2248 " else " \u2249 "
    names(l) + op + names(r)
  }

}