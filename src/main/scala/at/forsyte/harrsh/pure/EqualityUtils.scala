package at.forsyte.harrsh.pure

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.seplog.Var

/**
  * Created by jkatelaa on 10/17/16.
  */
object EqualityUtils extends HarrshLogging {

  def allEqualitiesOverVars(fvs : Seq[Var]) : Set[PureAtom] = {
    for {
      i <- Set() ++ fvs.indices
      j <- Set() ++ (i+1 until fvs.length)
      eq <- Set(true, false)
    } yield orderedAtom(fvs(i), fvs(j), eq)
  }

  def orderedAtom(left : Var, right : Var, isEqual : Boolean): PureAtom = {
    val (small, large) = if (left < right) (left, right) else (right, left)
    PureAtom(small, large, isEqual)
  }

  def orderedAtom(atom : PureAtom): PureAtom = {
    orderedAtom(atom.l, atom.r, atom.isEquality)
  }

  def varsEqualModuloPureSameSide(pure: Seq[PureAtom], fst: Var, snd: Var): Boolean = {
    logger.debug("Will check equality " + fst + " = " + snd + " modulo "+ pure)
    if (fst == snd) {
      // Syntactically the same => trivially equal
      true
    } else {
      // Not the same var => Have to check equalities
      val closure: Closure = Closure.ofAtoms(pure)
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
      val closureLhs: Closure = Closure.ofAtoms(pureLhs)
      val closureRhs: Closure = Closure.ofAtoms(pureRhs)
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
