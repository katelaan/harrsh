package at.forsyte.harrsh.pure

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{PtrEq, PtrNEq, PureAtom, SymbolicHeap}
import at.forsyte.harrsh.seplog.{PtrExpr, Var}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

/**
  * Created by jkatelaa on 10/17/16.
  */
object EqualityUtils extends HarrshLogging {

  def allEqualitiesOverFVs(numFV : Int) : Set[PureAtom] = {
    for {
      i <- Set() ++ (0 to numFV-1)
      j <- Set() ++ (i+1 to numFV)
      eq <- Set(true, false)
    } yield orderedAtom(Var(i), Var(j), eq)
  }

  def mkPure(atoms : (Int, Int, Boolean)*) : Set[PureAtom] = Set() ++ (atoms.toSeq map {
    case (l,r,isEq) => orderedAtom(Var(l),Var(r),isEq)
  })

  def unwrapAtom(atom : PureAtom) : (Var, Var, Boolean) = atom match {
    case PtrEq(l, r) => (l.getVarOrZero, r.getVarOrZero, true)
    case PtrNEq(l, r) => (l.getVarOrZero, r.getVarOrZero, false)
    case _ => throw new IllegalStateException("Heap automata are not defined on arithmetical expressions")
  }

  def orderedAtom(left : Var, right : Var, isEqual : Boolean): PureAtom = {
    val (small, large) = if (left < right) (left, right) else (right, left)
    if (isEqual) PtrEq(small, large) else PtrNEq(small, large)
  }

  def orderedAtom(atom : PureAtom): PureAtom = {
    val (left, right, isEqual) = unwrapAtom(atom)
    orderedAtom(left, right, isEqual)
  }



  def varsEqualModuloPureSameSide(pure: Seq[PureAtom], fst: Var, snd: Var): Boolean = {
    logger.debug("Will check equality " + fst + " = " + snd + " modulo "+ pure)
    if (fst == snd) {
      // Syntactically the same => trivially equal
      true
    } else {
      // Not the same var => Have to check equalities
      val closure: Closure = Closure.ofSetOfAtoms(pure.toSet)
      closure.getEquivalenceClass(fst).contains(snd)
    }
  }

  def varsEqualModuloPureDifferentSides(pureLhs: Seq[PureAtom], lhs: Var, pureRhs: Seq[PureAtom], rhs: Var): Boolean = {
    val msg = "Will check equality " + lhs + "%[" + pureLhs.mkString(",") + "] = " + rhs + "%[" + pureRhs.mkString(",") + "]"
    if (lhs == rhs) {
      // Syntactically the same => trivially equal
      true
    } else {
      // Not the same var => Have to check equalities
      val closureLhs: Closure = Closure.ofSetOfAtoms(pureLhs.toSet)
      val closureRhs: Closure = Closure.ofSetOfAtoms(pureRhs.toSet)
      val res = closureRhs.getEquivalenceClass(rhs).exists(closureLhs.getEquivalenceClass(lhs))
      if (res) {
        logger.debug(msg)
        logger.debug("RHS equality class " + closureRhs.getEquivalenceClass(rhs) + " contains " + closureRhs.getEquivalenceClass(rhs).find(closureLhs.getEquivalenceClass(lhs)) + " from LHS equality class " + closureLhs.getEquivalenceClass(lhs) + " => successful match")
      } else {
        logger.debug(msg + "... Equality check failed")
      }
      res
    }
  }

}
