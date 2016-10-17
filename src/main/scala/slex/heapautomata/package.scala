package slex

import seplog.{NullPtr, PtrExpr, PtrVar, PureAtom, PtrEq, PtrNEq}

/**
  * Created by jkatelaa on 10/17/16.
  */
package object heapautomata {

  val HeapAutomataSafeModeEnabled : Boolean = true

  type FV = PtrExpr

  val FVPrefix = "x"

  def fv(i : Int) : FV = if (i == 0) NullPtr() else PtrVar(FVPrefix + i)

  def isFV(fv : FV) = fv match {
    case NullPtr() => true
    case PtrVar(id) => id.startsWith(FVPrefix) // TODO: Should have a more sophisticated for "FV-ness" check here?
  }

  def unfv(fv : FV) : Int = fv match {
    case NullPtr() => 0
    case PtrVar(v) => Integer.parseInt(v.drop(FVPrefix.length))
  }

  def allEqualitiesOverFVs(numFV : Int) : Set[PureAtom] = {
    for {
      i <- Set() ++ (0 to numFV-1)
      j <- Set() ++ (i+1 to numFV)
      eq <- Set(true, false)
    } yield orderedAtom(fv(i), fv(j), eq)
  }

  def unwrapAtom(atom : PureAtom) : (FV, FV, Boolean) = atom match {
    case PtrEq(l, r) => (l, r, true)
    case PtrNEq(l, r) => (l, r, false)
    case _ => throw new IllegalStateException("Heap automata are not defined on arithmetical expressions")
  }

  def orderedAtom(left : PtrExpr, right : PtrExpr, isEqual : Boolean): PureAtom = {
    val (small, large) = if (left < right) (left, right) else (right, left)
    if (isEqual) PtrEq(small, large) else PtrNEq(small, large)
  }

  def orderedAtom(atom : PureAtom): PureAtom = {
    val (left, right, isEqual) = unwrapAtom(atom)
    val (small, large) = if (left < right) (left, right) else (right, left)
    if (isEqual) PtrEq(small, large) else PtrNEq(small, large)
  }

}
