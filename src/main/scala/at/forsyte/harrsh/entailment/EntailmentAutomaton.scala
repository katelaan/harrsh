package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Renaming}
import at.forsyte.harrsh.seplog.inductive._

/**
  * An entailment automaton for entailment instances phi |=_sid rhs, where sid and rhs are fixed.
  * @param sid The SID definining the meaning of the rhs.
  * @param rhs The predicate call on the right-hand side of the entailment.
  */
class EntailmentAutomaton(sid: SID, rhs: PredCalls) extends HeapAutomaton with InconsistentState {

  // The entailment checker only works for rooted SIDs that satisfy progress
  // TODO: Check rootedness and progress for the part of the SID that does not correspond to the top-level predicate?
  //assert(sid.isRooted)
  //assert(sid.satisfiesProgress)

  //if (sid.preds.flatMap(_.rules).exists(!_.hasPointer)) throw new IllegalArgumentException("For the right-hand side of the entailment there is currently no support for rules that don't allocate memory")

  override val description: String = s"EntailmentAutomaton($rhs)"

  override type State = EntailmentProfile

  override def isFinal(s: State): Boolean = {
    // A state represents all the possible ways to parse an RSH as a SID unfolding tree.
    // As long as one of those ways is a valid unfolding tree, we accept.
    val res = s.decomps.exists(_.isFinal(rhs))
    logger.trace(s"Checked wheter $s is final => $res")
    res
  }

  override def inconsistentState(fvs: Seq[FreeVar]): State = EntailmentAutomaton.InconsistentState(fvs)

  override def getTargetsFor(src: Seq[State], lab: SymbolicHeap) : Set[State] = {
    logger.debug(s"Computing target for $lab from source states:\n${src.mkString("\n")}")
    TargetProfile(src, lab, sid).get.toSet
  }

}

object EntailmentAutomaton extends HarrshLogging {

  /**
    * An inconsistent state representing all "sink" states of the given parameter sequence
    */
  def InconsistentState(orderedParams: Seq[FreeVar]) = EntailmentProfile(Set.empty, orderedParams)

  private def rename(sh: SymbolicHeap, perm: Seq[FreeVar]): SymbolicHeap = {
    val renamingPairs = sh.freeVars.zip(perm)
    val renaming = Renaming.fromPairs(renamingPairs)
    sh.rename(renaming, overrideFreeVars = None)
  }
}
