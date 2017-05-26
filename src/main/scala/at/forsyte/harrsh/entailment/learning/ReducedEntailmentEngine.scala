package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment.ReducedEntailment
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jkatelaa on 5/26/17.
  */
trait ReducedEntailmentEngine {

  val learningLog : EntailmentLearningLog

  val reportProgress : Boolean

  def reducedEntailment(lhs: SymbolicHeap, rhs: SymbolicHeap, sid: SID, reason: EntailmentLearningLog.RedEntCheck.CheckPurpose): Boolean = {
    learningLog.logEvent(EntailmentLearningLog.RedEntCheck(lhs, rhs, reason))
    ReducedEntailment.checkSatisfiableRSHAgainstSID(lhs, rhs, sid, reportProgress = reportProgress)
  }

}
