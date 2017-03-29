package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/29/17.
  */
trait FVBound {

  this : HeapAutomaton =>

    val numFV : Int

    override final def doesAlphabetContain(lab: SymbolicHeap): Boolean = lab.numFV <= numFV

}
