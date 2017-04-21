package at.forsyte.harrsh.seplog

import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait PtrExpr extends Expr with ToStringWithVarnames {

  override def toStringWithVarNames(names: VarNaming) = this match {
    case NullPtr() => "null"
    case PtrVar(id) => names(id)
  }

  def getVar : Set[Var] = this match {
    case NullPtr() => Set()
    case PtrVar(id) => Set(id)
  }

  def getVarOrZero : Var = this match {
    case NullPtr() => 0
    case PtrVar(id) => id
  }

  def getVarUnsafe : Var = this match {
    case NullPtr() => throw new Throwable("Tried to convert null pointer to variable")
    case PtrVar(id) => id
  }

  def <(other : PtrExpr) : Boolean = (this, other) match {
    case (NullPtr(), NullPtr()) => false
    case (NullPtr(), _) => true
    case (_, NullPtr()) => false
    case (PtrVar(l), PtrVar(r)) => l < r
  }

  def renameVars(f : Renaming) : PtrExpr = this match {
    case n : NullPtr => n
    case PtrVar(id) => PtrVar(f(id))
  }

}

case class NullPtr() extends PtrExpr

case class PtrVar(id : Var) extends PtrExpr

object PtrExpr {

  def fromFV(x : Var) : PtrExpr = if (x == 0) NullPtr() else PtrVar(x)

}