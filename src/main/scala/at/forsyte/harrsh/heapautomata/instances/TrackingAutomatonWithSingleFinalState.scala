package at.forsyte.harrsh.heapautomata.instances

import at.forsyte.harrsh.heapautomata.utils.TrackingInfo
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom

/**
  * Tracking automaton for the given number of free variables, whose final state is specified by alloc and pure.
  */
class TrackingAutomatonWithSingleFinalState(numFV: Int, alloc : Set[Var], pure : Set[PureAtom]) extends BaseTrackingAutomaton(numFV) {

  override val description = "TRACK_" + numFV + "(" + alloc + ", " + pure + ")"

  override def isFinal(s: TrackingInfo) = s.pure == pure && s.alloc == alloc

}
