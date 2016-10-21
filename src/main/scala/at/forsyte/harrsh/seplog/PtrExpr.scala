package at.forsyte.harrsh.seplog

/**
  * Created by jkatelaa on 9/30/16.
  */
sealed trait PtrExpr extends Expr {

  override def toString = this match {
    case NullPtr() => "null"
    case PtrVar(id) => id
  }

  def getIdentOptionMDEC : Option[String] = this match {
    case NullPtr() => Some("null") // FIXME [NULL-TREATMENT] Currently we handle null by declaring it as an ordinary constant. Might want to fix this at some point
    case PtrVar(id) => Some(id)
  }

  def getVar : Set[String] = this match {
    case NullPtr() => Set()
    case PtrVar(id) => Set(id)
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

case class PtrVar(id : String) extends PtrExpr

object PtrExpr {

  def fromString(s : String) : PtrExpr = s match {
    case "null" => NullPtr()
    case "nil" => NullPtr()
    case _ => PtrVar(s)
  }

}