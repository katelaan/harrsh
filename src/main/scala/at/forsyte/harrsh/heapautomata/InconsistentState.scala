package at.forsyte.harrsh.heapautomata

/**
  * Created by jens on 3/29/17.
  */
trait InconsistentState {

  this : HeapAutomaton =>

  val inconsistentState : State

}
