package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.main.SlexLogging
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Base class for heap automata families parameterized by the number of FVs that implement target computation
  * from the source sequence and the symbolic heap.
  * Includes default implementation of isTransitionDefined in terms of the target computation.
  *
  * Created by jkatelaa on 10/18/16.
  */
abstract class BoundedFvAutomatonWithTargetComputation(numFV : Int) extends HeapAutomaton with SlexLogging {

  override final def doesAlphabetContain(lab: SymbolicHeap): Boolean = getMaxFvIndex(lab.getVars) <= numFV

  override def isTransitionDefined(src: Seq[State], trg: State, lab: SymbolicHeap): Boolean = {
    val targets = getTargetsFor(src, lab)
    val res = targets.contains(trg)
    logger.debug("Transition " + src.mkString(", ") + " --[" + lab + "]--> " + trg + " : " + res)
    res
  }

  override final def implementsTargetComputation: Boolean = true

}
