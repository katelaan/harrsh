package at.forsyte.harrsh.heapautomata

/**
  * Created by jens on 3/29/17.
  */
trait TargetComputation {

  this : HeapAutomaton =>

  override final def implementsTargetComputation: Boolean = true

}
