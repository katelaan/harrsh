package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.refinement.AutomatonTask

/**
  * Created by jens on 3/29/17.
  */
class SatAutomaton(numFV : Int, negate : Boolean) extends BaseTrackingAutomaton(numFV) {

  override val description = (if (negate) AutomatonTask.keywords.unsat else AutomatonTask.keywords.sat) + "_" + numFV

  override def isFinal(s : State) : Boolean = negate != s.isConsistent

}
