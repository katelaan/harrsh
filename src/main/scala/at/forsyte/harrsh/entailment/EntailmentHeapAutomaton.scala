package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.BoundedFvAutomatonWithTargetComputation
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/6/17.
  */
class EntailmentHeapAutomaton(numFV : Int, ecds : Seq[(SymbolicHeap, SymbolicHeap,Boolean)]) extends BoundedFvAutomatonWithTargetComputation(numFV) {

  val stateMap : Map[Int,(SymbolicHeap,SymbolicHeap,Boolean)] = Map() ++ ecds.zipWithIndex.map(_.swap)

  override type State = Int
  override lazy val states: Set[State] = stateMap.keySet

  override def isFinal(s: State): Boolean = stateMap(s)._3

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    // Shrinking + entailment check(s)

    // Important: Perform entailment checks in reverse order!
    // TODO Figure out the details. The point is: The ECDs we find later might share some extension behavior with the ones we've found before, so we might mistakenly go to the wrong state if we start by checking the ECDs we discover first

    throw new NotImplementedError("No smart target computation for this heap automaton")
  }

  override val InconsistentState: Int = ???
}

object EntailmentHeapAutomaton {

  def serialize(aut: EntailmentHeapAutomaton) : String = ???

  def fromString(s : String) : EntailmentHeapAutomaton = ???

}
