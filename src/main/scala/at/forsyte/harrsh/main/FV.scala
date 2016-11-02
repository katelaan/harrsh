package at.forsyte.harrsh.main

import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.seplog.{NullPtr, PtrVar}

/**
  * Created by jens on 11/2/16.
  */

object FV {

  val FreeVarString = "x"

  val BoundVarString = "y"

  def getFirstBoundVar: FV = -1

  @inline def fv(i : Int) : FV = i

  @inline def isFV(fv : FV) = fv >= 0

  @inline def stringToFV(fv : String) : FV = if (fv.startsWith(FreeVarString)) Integer.valueOf(fv.drop(FreeVarString.length)) else throw new IllegalArgumentException("Passed non-free variable identifier to free-variable conversion")

  def varToDefaultString(fv : FV) = fv match {
    case 0 => NullPtr().toString
    case i if i > 0 => FreeVarString + i
    case i => BoundVarString + (-i)
  }

  def isFV(fv : String) = fv match {
    case "null" => true
    case "nil" => true
    case id => id.startsWith(FreeVarString) // TODO: Should have a more sophisticated for "FV-ness" check here?
  }

  @inline def unFV(fv : FV) : Int = fv

//  def unFV(fv : String) : Int = fv match {
//    case "null" => 0
//    case "nil" => 0
//    case v => Integer.parseInt(v.drop(FVPrefix.length))
//  }

  @inline def fvAll(ints : Int*) : Set[FV] = Set() ++ ints map fv

  @inline def getMaxFvIndex(vars : Set[FV]) : Int = vars.max



}
