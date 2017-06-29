package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{ConsistencyCheck, Determinization}
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jkatelaa on 5/16/17.
  */
object ReducedEntailment extends HarrshLogging {

  def checkSatisfiableRSHAgainstSID(lhsRsh : SymbolicHeap, rhs : SymbolicHeap, sid : SID, reportProgress : Boolean = false) : Boolean = {
    assert(ConsistencyCheck.isConsistent(lhsRsh))
    checkForAllDeterminizations(lhsRsh, rhs, sid, reportProgress = reportProgress)
  }

  def checkSatisfiableRSHAgainstRSH(lhsRsh : SymbolicHeap, rhsRsh : SymbolicHeap, reportProgress : Boolean = false) : Boolean = {
    assert(ConsistencyCheck.isConsistent(lhsRsh))
    assert(rhsRsh.isReduced)
    checkForAllDeterminizations(lhsRsh, rhsRsh, SID.empty("X"), reportProgress = reportProgress)
  }

  def checkPossiblyInconsistentRSHAgainstSID(lhsRsh : SymbolicHeap, rhs : SymbolicHeap, sid : SID, reportProgress : Boolean = false) : Boolean = {
    if (!ConsistencyCheck.isConsistent(lhsRsh))
      true
    else
      checkForAllDeterminizations(lhsRsh, rhs, sid, reportProgress = reportProgress)
  }

  def checkPossiblyInconsistentRSHAgainstRSH(lhsRsh : SymbolicHeap, rhsRsh : SymbolicHeap, reportProgress : Boolean = false) : Boolean = {
    assert(rhsRsh.isReduced)
    if (!ConsistencyCheck.isConsistent(lhsRsh))
      true
    else
      checkForAllDeterminizations(lhsRsh, rhsRsh, SID.empty("X"), reportProgress = reportProgress)
  }

  private def checkForAllDeterminizations(lhs : SymbolicHeap, rhs : SymbolicHeap, sid : SID, reportProgress: Boolean = false): Boolean = {

    val determinizations = Determinization.rshDeterminizations(lhs)
    val msg = "Applying reduced model checking to " + determinizations.size + " determinizations "
    logger.info(msg)
    if (reportProgress && determinizations.size > 1) {
      println(msg)
    }

    val resultsForDeterminizations = determinizations map {
      lhs =>
        val res = if (!ConsistencyCheck.isConsistent(lhs)) true else GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhs, rhs, sid, reportProgress = reportProgress)
        logger.debug("Checking determinization " + lhs + " |= " + rhs + " --> " + res)
        res
    }
    resultsForDeterminizations.forall(b => b)
  }

}
