package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Base class for heap automata families parameterized by the number of FVs that implement target computation
  * from the source sequence and the symbolic heap.
  *
  * Created by jkatelaa on 10/18/16.
  */
abstract class BoundedFvAutomatonWithTargetComputation(numFV : Int) extends HeapAutomaton {

  override final def doesAlphabetContain(lab: SymbolicHeap): Boolean = lab.numFV <= numFV

  override final def implementsTargetComputation: Boolean = true

}
