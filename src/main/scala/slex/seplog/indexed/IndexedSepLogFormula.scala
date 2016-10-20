package slex.seplog.indexed

import slex.seplog.Renaming
import slex.util.Combinators

/**
  * Created by jkatelaa on 9/30/16.
  */
trait IndexedSepLogFormula {

  def isSpatial : Boolean

  def isPure : Boolean

  def isSymbolicHeap : Boolean

  def toSymbolicHeap : Option[IndexedSymbolicHeap]

  def renameVars(f : Renaming) : IndexedSepLogFormula
}

case class Neg(phi : IndexedSepLogFormula) extends IndexedSepLogFormula {
  override def toString = "(\u00ac " + phi + ")"

  override def isSpatial: Boolean = phi.isSpatial

  override def isPure: Boolean = phi.isPure

  // Symbolic heaps are not closed under negation
  override def isSymbolicHeap: Boolean = false

  override def toSymbolicHeap: Option[IndexedSymbolicHeap] = None

  override def renameVars(f : Renaming) = Neg(phi.renameVars(f))
}

case class And(phi : IndexedSepLogFormula, psi : IndexedSepLogFormula) extends IndexedSepLogFormula {
  override def toString = "(" + phi + " \u2227 " + psi + ")"

  override def isSpatial: Boolean = phi.isSpatial || psi.isSpatial

  override def isPure: Boolean = phi.isPure && psi.isPure

  // The conjunction of two pure formulas is a symbolic heap
  override def isSymbolicHeap: Boolean = isPure && phi.isSymbolicHeap && psi.isSymbolicHeap

  override def toSymbolicHeap: Option[IndexedSymbolicHeap] = IndexedSymbolicHeap.combineHeaps(phi.toSymbolicHeap, psi.toSymbolicHeap)

  override def renameVars(f : Renaming) = And(phi.renameVars(f), psi.renameVars(f))
}

case class Or(phi : IndexedSepLogFormula, psi : IndexedSepLogFormula) extends IndexedSepLogFormula {
  override def toString = "(" + phi + " \u2228 " + psi + ")"

  override def isSpatial: Boolean = phi.isSpatial || psi.isSpatial

  override def isPure: Boolean = phi.isPure && psi.isPure

  // Symbolic heaps are not closed under disjunction
  override def isSymbolicHeap: Boolean = false

  override def toSymbolicHeap: Option[IndexedSymbolicHeap] = None

  override def renameVars(f : Renaming) = Or(phi.renameVars(f), psi.renameVars(f))
}

case class Exists(varid : String, phi : IndexedSepLogFormula) extends IndexedSepLogFormula {
  override def toString = "(\u2203 "  + varid + " . " + phi + ")"

  override def isSpatial: Boolean = phi.isSpatial

  override def isPure: Boolean = phi.isPure

  override def isSymbolicHeap: Boolean = phi.isSymbolicHeap

  override def toSymbolicHeap: Option[IndexedSymbolicHeap] = phi.toSymbolicHeap map {
    case IndexedSymbolicHeap(pure, spatial, qvars) => IndexedSymbolicHeap(pure, spatial, varid :: qvars.toList)
  }

  override def renameVars(f : Renaming) = {
    val extended = f.addBoundVarWithOptionalAlphaConversion(varid)
    Exists(extended(varid), phi.renameVars(extended))
  }
}

case class SepCon(phi : IndexedSepLogFormula, psi : IndexedSepLogFormula) extends IndexedSepLogFormula {
  override def toString = "(" + phi + " * " + psi + ")"

  override def isSpatial: Boolean = phi.isSpatial || psi.isSpatial

  // * of two pure formulas is equivalent to their conjunction
  override def isPure: Boolean = phi.isPure && psi.isPure

  // The separating conjunction of two symbolic heaps is a symbolic heap
  override def isSymbolicHeap: Boolean = phi.isSymbolicHeap && psi.isSymbolicHeap

  override def toSymbolicHeap: Option[IndexedSymbolicHeap] = IndexedSymbolicHeap.combineHeaps(phi.toSymbolicHeap, psi.toSymbolicHeap)

  override def renameVars(f : Renaming) = SepCon(phi.renameVars(f), psi.renameVars(f))
}

object IndexedSepLogFormula {

  def fromPureAtoms(atoms : Seq[IndexedPureAtom]) : PureFormula = Combinators.iteratedBinOp[PureFormula](PureAnd, True())(atoms)

  def fromSpatialAtoms(atoms : Seq[IndexedSpatialAtom]) : IndexedSepLogFormula = Combinators.iteratedBinOp[IndexedSepLogFormula](SepCon, Emp())(atoms)

}