package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.seplog.FreeVar

/**
  * Created by jens on 3/29/17.
  */
trait InconsistentState {

  this : HeapAutomaton =>

  /**
    * An inconsistent state representing all "sink" states
    */
  def inconsistentState(fvs: Seq[FreeVar]) : State

  /**
    * Is it possible for a transition with one or more inconsistent source states to lead to a consistent state?
    */
  val canRecoverFromInconsistentState = true
  // TODO: Use that flag in on the fly refinement? See also SatAutomaton

}
