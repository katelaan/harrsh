package at.forsyte.harrsh.seplog

/**
  * Created by jens on 11/2/16.
  */

case class Var(private val underlying : Int) extends AnyVal {

  def toInt = underlying

  def isFreeNonNull : Boolean = underlying > 0
  def isFree : Boolean = underlying >= 0
  def isBound : Boolean = underlying < 0
  def isNull : Boolean = underlying == 0

  override def toString : String = underlying match {
    case 0 => NullPtr.toString
    case i if i > 0 => Var.FreeVarString + i
    case i => Var.BoundVarString + (-i)
  }

  def <(other : Var) = underlying < other.underlying
  def >(other : Var) = underlying > other.underlying

  def +(i : Int) = Var(underlying + i)
  def -(i : Int) = Var(underlying - i)

}

object Var {

  val FreeVarString = "x"

  val BoundVarString = "y"

  val nil : Var = Var(0)

  def isFreeVariableString(fv : String) = fv match {
    case "null" => true
    case "nil" => true
    case id => id.startsWith(FreeVarString)
  }

  @inline def stringToFV(fv : String) : Var =
    if (fv.startsWith(FreeVarString)) Var(Integer.valueOf(fv.drop(FreeVarString.length)))
    else if (fv == "null" || fv == "nil") Var(0)
    else throw new IllegalArgumentException("Passed non-free variable identifier to free-variable conversion")

  @inline def mkAllVars(ints : Seq[Int]) : Seq[Var] = ints map (Var(_))

  @inline def mkSetOfAllVars(ints : Iterable[Int]) : Set[Var] = Set() ++ ints map (Var(_))

  @inline def maxOf(vars : Iterable[Var]) : Var = Var(vars.map(_.underlying).max)

  @inline def minOf(vars : Iterable[Var]) : Var = Var(vars.map(_.underlying).min)

}
