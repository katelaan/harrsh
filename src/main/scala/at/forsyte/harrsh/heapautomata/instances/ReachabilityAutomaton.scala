package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.ReachabilityInfo
import at.forsyte.harrsh.seplog.Var

/**
  * Created by jens on 3/29/17.
  */
class ReachabilityAutomaton(override val numFV : Int, from : Var, to : Var) extends BaseReachabilityAutomaton(numFV) {
  assert(from <= numFV && to <= numFV)

  override def isFinal(s : ReachabilityInfo) = s.rm.isReachable(from, to)
  override val description = "REACH_" + numFV

}
