package at.forsyte.harrsh.pure

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PtrNEq, PureAtom}

/**
  * Created by jkatelaa on 5/16/17.
  */
object PureEntailment extends HarrshLogging {

  def check(lhs: Seq[PureAtom], rhs: Seq[PureAtom]): Boolean = {
    logger.debug("Checking pure entailment\n     " + lhs.mkString(", ") + "\n |?= " + rhs.mkString(", "))

    val lhsClosure: Closure = Closure.ofSetOfAtoms(lhs.toSet)
    logger.trace("Closure of lhs: " + lhsClosure.asSetOfAtoms)

    if (lhsClosure.isConsistent) {
      val rhsClosure: Closure = Closure.ofSetOfAtoms(rhs.toSet)
      logger.trace("Closure of rhs: " + rhsClosure.asSetOfAtoms)

      // TODO Actually there is no need to compute the closure explicitly, should improve this at some point
      // Every equality and disequality in the rhs should be respected by the lhs, apart from explicit equalities of the kind x == x
      def notTautology(atom: PureAtom) = atom.isInstanceOf[PtrNEq] || atom.getVarsWithNull.size == 2

      // TODO The following is not robust, since I rely on the implementation-specific detail that the closure returns ordered atoms (in each atom, smaller var on the left)
      val lhsClosureSet = lhsClosure.asSetOfAtoms filter notTautology
      val rhsClosureSet = rhsClosure.asSetOfAtoms filter notTautology

      // After we've computed the closure, we can throw away existentially quantified variables:
      // The (existentially!) quantified variables do not strengthen the constraints in any way.
      val lhsFreeClosureSet = lhsClosureSet filterNot (atom => atom.getVars.exists(_.isBound))
      val rhsFreeClosureSet = rhsClosureSet filterNot (atom => atom.getVars.exists(_.isBound))

      logger.debug(rhsFreeClosureSet + " subset of " + lhsFreeClosureSet + "?")
      val res = rhsFreeClosureSet subsetOf lhsFreeClosureSet
      if (res) logger.trace("Result of subset test: " + res) else logger.debug("Not subset: LHS does not contain " + (rhsFreeClosureSet -- lhsFreeClosureSet))
      res
    } else {
      // If the LHS is inconsistent, it entails anything
      logger.debug("LHS constraints " + lhs.mkString("{", ",", "}") + " are inconsistent, entailment holds vacuously")
      true
    }
  }

}
