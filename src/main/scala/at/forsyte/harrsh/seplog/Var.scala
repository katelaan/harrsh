package at.forsyte.harrsh.seplog

/**
  * Created by jens on 11/2/16.
  */

object Var {

  val FreeVarString = "x"

  val BoundVarString = "y"

  def getFirstBoundVar: Var = -1

  @inline def mkVar(i : Int) : Var = i

  @inline def isFV(fv : Var) = fv >= 0

  @inline def isBound(fv : Var) = fv < 0

  def isFV(fv : String) = fv match {
    case "null" => true
    case "nil" => true
    case id => id.startsWith(FreeVarString) // TODO: Should have a more sophisticated for "FV-ness" check here?
  }

  @inline def stringToFV(fv : String) : Var =
    if (fv.startsWith(FreeVarString)) Integer.valueOf(fv.drop(FreeVarString.length))
    else if (fv == "null" || fv == "nil") 0
    else throw new IllegalArgumentException("Passed non-free variable identifier to free-variable conversion")

  def toDefaultString(v : Var) = v match {
    case 0 => NullPtr().toString
    case i if i > 0 => FreeVarString + i
    case i => BoundVarString + (-i)
  }

  @inline def unVar(v : Var) : Int = v

  @inline def mkAllVar(ints : Int*) : Set[Var] = Set() ++ ints map mkVar

  @inline def getMaxVarIndex(vars : Set[Var]) : Int = vars.max

  @inline def mkAllFVs(numFV : Int) = (0 to numFV) map mkVar


}
