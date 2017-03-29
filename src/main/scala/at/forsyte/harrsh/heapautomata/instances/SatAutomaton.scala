package at.forsyte.harrsh.heapautomata.instances

/**
  * Created by jens on 3/29/17.
  */
class SatAutomaton(numFV : Int, negate : Boolean) extends BaseTrackingAutomaton(numFV) {

  override val description = (if (negate) "UNSAT_" else "SAT_") + numFV

  override def isFinal(s : State) : Boolean = negate != s.isConsistent

}
