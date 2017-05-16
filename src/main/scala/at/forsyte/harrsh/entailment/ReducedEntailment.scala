package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.pure.ConsistencyCheck
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jkatelaa on 5/16/17.
  */
object ReducedEntailment {

  def checkSatisfiableRSHAgainstSID(lhsRsh : SymbolicHeap, rhs : SymbolicHeap, sid : SID, reportProgress : Boolean = false) : Boolean = {
    assert(ConsistencyCheck.isConsistent(lhsRsh))
    GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhsRsh, rhs, sid, reportProgress = reportProgress)
  }

  def checkSatisfiableRSHAgainstRSH(lhsRsh : SymbolicHeap, rhsRsh : SymbolicHeap, reportProgress : Boolean = false) : Boolean = {
    assert(ConsistencyCheck.isConsistent(lhsRsh))
    assert(rhsRsh.predCalls.isEmpty)
    GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhsRsh, rhsRsh, SID.empty("X"), reportProgress = reportProgress)
  }

  def checkPossiblyInconsistentRSHAgainstSID(lhsRsh : SymbolicHeap, rhs : SymbolicHeap, sid : SID, reportProgress : Boolean = false) : Boolean = {
    if (!ConsistencyCheck.isConsistent(lhsRsh))
      true
    else
      GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhsRsh, rhs, sid, reportProgress = reportProgress)
  }

  def checkPossiblyInconsistentRSHAgainstRSH(lhsRsh : SymbolicHeap, rhsRsh : SymbolicHeap, reportProgress : Boolean = false) : Boolean = {
    assert(rhsRsh.predCalls.isEmpty)
    if (!ConsistencyCheck.isConsistent(lhsRsh))
      true
    else
      GreedyUnfoldingModelChecker.reducedEntailmentAsModelChecking(lhsRsh, rhsRsh, SID.empty("X"), reportProgress = reportProgress)
  }

}
