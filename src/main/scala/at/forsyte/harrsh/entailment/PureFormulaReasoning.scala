package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.utils.{Closure, ClosureOfAtomSet}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PtrNEq, PureAtom}

/**
  * Created by jkatelaa on 3/24/17.
  */
object PureFormulaReasoning extends HarrshLogging {

  def varsEqualModuloPureSameSide(pure: Seq[PureAtom], fst: Var, snd: Var): Boolean = {
    logger.debug("Will check equality " + fst + " = " + snd + " modulo "+ pure)
    if (fst == snd) {
      // Syntactically the same => trivially equal
      true
    } else {
      // Not the same var => Have to check equalities
      val closure: Closure = new ClosureOfAtomSet(pure.toSet)
      closure.getEqualityClass(fst).contains(snd)
    }
  }

  def varsEqualModuloPureDifferentSides(pureLhs: Seq[PureAtom], lhs: Var, pureRhs: Seq[PureAtom], rhs: Var): Boolean = {
    val msg = "Will check equality " + lhs + "%[" + pureLhs.mkString(",") + "] = " + rhs + "%[" + pureRhs.mkString(",") + "]"
    if (lhs == rhs) {
      // Syntactically the same => trivially equal
      true
    } else {
      // Not the same var => Have to check equalities
      val closureLhs: Closure = new ClosureOfAtomSet(pureLhs.toSet)
      val closureRhs: Closure = new ClosureOfAtomSet(pureRhs.toSet)
      val res = closureRhs.getEqualityClass(rhs).exists(closureLhs.getEqualityClass(lhs))
      if (res) {
        logger.debug(msg)
        logger.debug("RHS equality class " + closureRhs.getEqualityClass(rhs) + " contains " + closureRhs.getEqualityClass(rhs).find(closureLhs.getEqualityClass(lhs)) + " from LHS equality class " + closureLhs.getEqualityClass(lhs) + " => successful match")
      } else {
        logger.debug(msg + "... Equality check failed")
      }
      res
    }
  }

  // FIXME Is this the correct entailment check?
  def pureFormulaEntailment(lhs: Seq[PureAtom], rhs: Seq[PureAtom], reportProgress : Boolean = false): Boolean = {
    logger.debug("Checking pure entailment\n     " + lhs.mkString(", ") + "\n |?= " + rhs.mkString(", "))
    if (reportProgress) println("    Checking pure entailment " + lhs.mkString(", ") + " |?= " + rhs.mkString(", "))

    val lhsClosure: Closure = new ClosureOfAtomSet(lhs.toSet)
    val rhsClosure: Closure = new ClosureOfAtomSet(rhs.toSet)

    // TODO Actually there is no need to compute the closure explicitly, should improve this at some point
    // Every equality and disequality in the rhs should be respected by the lhs, apart from explicit equalities of the kind x == x
    def notTautology(atom: PureAtom) = atom.isInstanceOf[PtrNEq] || atom.getVarsWithNull.size == 2

    val lhsClosureSet = lhsClosure.asSetOfAtoms filter notTautology
    val rhsClosureSet = rhsClosure.asSetOfAtoms filter notTautology

    // After we've computed the closure, we can throw away existentially quantified variables:
    // The (existentially!) quantified variables do not strengthen the constraints in any way.
    val lhsFreeClosureSet = lhsClosureSet filterNot (atom => atom.getVars.exists(Var.isBound(_)))
    val rhsFreeClosureSet = rhsClosureSet filterNot (atom => atom.getVars.exists(Var.isBound(_)))

    logger.debug(rhsFreeClosureSet + " subset of " + lhsFreeClosureSet + "?")
    rhsFreeClosureSet subsetOf lhsFreeClosureSet
  }

}
