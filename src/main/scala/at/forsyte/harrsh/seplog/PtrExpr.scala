package at.forsyte.harrsh.seplog

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
    case NullPtr() => Var.nil
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
    case PtrVar(id) => PtrExpr.fromFV(f(id))
  }

}

case class NullPtr private () extends PtrExpr

case class PtrVar private (id : Var) extends PtrExpr {
  assert(id != Var.nil)
}

object PtrExpr {

  def fromFV(x : Var) : PtrExpr = if (x == Var.nil) NullPtr() else PtrVar(x)

}