package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 10/19/16.
  */
trait TaggedTargetComputation[A] extends TargetComputation {

  this : HeapAutomaton =>

    override final def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = getTargetsWithTags(src, lab) map (_._1)

    def getTargetsWithTags(src : Seq[State], lab : SymbolicHeap) : Set[(State,A)]

}
