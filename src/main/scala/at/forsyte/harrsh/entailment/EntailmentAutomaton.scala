package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.HeapAutomaton.Transition
import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Renaming, Var}
import at.forsyte.harrsh.seplog.inductive._

/**
  * An entailment automaton for entailment instances phi |=_sid rhs, where sid and rhs are fixed.
  * @param sid The SID definining the meaning of the rhs.
  * @param rhs The predicate call on the right-hand side of the entailment.
  */
class EntailmentAutomaton(sid: RichSid, rhs: TopLevelConstraint) extends HeapAutomaton with InconsistentState {

  override val description: String = s"EntailmentAutomaton($rhs)"

  override type State = EntailmentProfile

  override def isFinal(s: State): Boolean = {
    s.isFinal(sid, rhs)
  }

  override def inconsistentState(fvs: Seq[FreeVar]): State = EntailmentAutomaton.InconsistentState(fvs.toSet)

  override def getTargetsFor(src: Seq[State], lab: SymbolicHeap) : Set[State] = {
    logger.debug(s"Computing target for $lab from source states:\n${src.mkString("\n")}")
    getTransitionsFor(src, lab, head="DON'T CARE").map(_.headState)
  }

  override def getTransitionsFor(src : Seq[State], lab : SymbolicHeap, head: String) : Set[Transition[EntailmentProfile]] = {
    logger.debug(s"Computing target for $lab from source states:\n${src.mkString("\n")}")
    TargetProfile(src, lab, sid).getTransition(lab, head).toSet
  }

}

object EntailmentAutomaton extends HarrshLogging {

  /**
    * An inconsistent state representing all "sink" states of the given parameter sequence
    */
  def InconsistentState(params: Set[Var]) = ProfileOfNondecomposableModels(params)

  private def rename(sh: SymbolicHeap, perm: Seq[FreeVar]): SymbolicHeap = {
    val renamingPairs = sh.freeVars.zip(perm)
    val renaming = Renaming.fromPairs(renamingPairs)
    sh.rename(renaming, overrideFreeVars = None)
  }
}
