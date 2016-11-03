package at.forsyte.harrsh

import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive._

/**
  * Created by jkatelaa on 10/17/16.
  */
package object heapautomata {

  val HeapAutomataSafeModeEnabled : Boolean = false

  import Var._

  def allEqualitiesOverFVs(numFV : Int) : Set[PureAtom] = {
    for {
      i <- Set() ++ (0 to numFV-1)
      j <- Set() ++ (i+1 to numFV)
      eq <- Set(true, false)
    } yield orderedAtom(mkVar(i), mkVar(j), eq)
  }

  def mkPure(atoms : (Int, Int, Boolean)*) : Set[PureAtom] = Set() ++ (atoms.toSeq map {
    case (l,r,isEq) => orderedAtom(mkVar(l),mkVar(r),isEq)
  })

  def unwrapAtom(atom : PureAtom) : (Var, Var, Boolean) = atom match {
    case PtrEq(l, r) => (l.getVarOrZero, r.getVarOrZero, true)
    case PtrNEq(l, r) => (l.getVarOrZero, r.getVarOrZero, false)
    case _ => throw new IllegalStateException("Heap automata are not defined on arithmetical expressions")
  }

  def orderedAtom(left : Var, right : Var, isEqual : Boolean): PureAtom = {
    val (small, large) = if (left < right) (left, right) else (right, left)
    if (isEqual) PtrEq(PtrExpr.fromFV(small), PtrExpr.fromFV(large)) else PtrNEq(PtrExpr.fromFV(small), PtrExpr.fromFV(large))
  }

  def orderedAtom(atom : PureAtom): PureAtom = {
    val (left, right, isEqual) = unwrapAtom(atom)
    orderedAtom(left, right, isEqual)
  }

}
