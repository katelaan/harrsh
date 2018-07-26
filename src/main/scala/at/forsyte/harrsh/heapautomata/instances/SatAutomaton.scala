package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.refinement.AutomatonTask

/**
  * Created by jens on 3/29/17.
  */
class SatAutomaton(negate : Boolean) extends BaseTrackingAutomaton {

  override val description = (if (negate) AutomatonTask.keywords.unsat else AutomatonTask.keywords.sat)

  override def isFinal(s : State) : Boolean = negate != s.isConsistent

  override def isNonSink(s : State) : Boolean = s.isConsistent || negate

  //override val canRecoverFromInconsistentState = false

}
