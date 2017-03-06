package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.BoundedFvAutomatonWithTargetComputation

/**
  * Created by jens on 3/6/17.
  */
abstract class EntailmentHeapAutomaton(numFV : Int) extends BoundedFvAutomatonWithTargetComputation(numFV) {

}

object EntailmentHeapAutomaton {

  def serialize(aut: EntailmentHeapAutomaton) : String = ???

  def fromString(s : String) : EntailmentHeapAutomaton = ???

}
