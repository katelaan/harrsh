package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.ReachabilityInfo
import at.forsyte.harrsh.refinement.AutomatonTask
import at.forsyte.harrsh.seplog.Var

/**
  * Created by jens on 3/29/17.
  */
class ReachabilityAutomaton(from : Var, to : Var, negate : Boolean = false) extends BaseReachabilityAutomaton {

  override def isFinal(s : ReachabilityInfo) = s.rm.isReachable(from, to) != negate
  override val description = AutomatonTask.keywords.reach

}
