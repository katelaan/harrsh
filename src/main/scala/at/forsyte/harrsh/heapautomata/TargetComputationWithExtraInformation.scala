package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 10/19/16.
  */

trait TargetComputationWithExtraInformation[A] {

  this : BoundedFvAutomatonWithTargetComputation =>

    def getTargetsWithExtraInfo(src : Seq[State], lab : SymbolicHeap) : Set[(State,A)]

}
