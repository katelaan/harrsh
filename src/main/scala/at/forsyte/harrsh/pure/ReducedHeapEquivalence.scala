package at.forsyte.harrsh.pure

import at.forsyte.harrsh.entailment.{GreedyUnfoldingModelChecker, ReducedEntailment}
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jens on 5/3/17.
  */
object ReducedHeapEquivalence {

  def apply(fst : SymbolicHeap, snd : SymbolicHeap, reportProgress : Boolean = false) : Boolean = {
    // FIXME This only works for garbage-free symbolic heaps due to reliance on pointer unification for garbage-free heaps; might want to generalize!
    // TODO More efficient implementation of RSH equivalence...
    /*fst == snd ||*/(ReducedEntailment.checkPossiblyInconsistentRSHAgainstRSH(fst, snd, reportProgress) && ReducedEntailment.checkPossiblyInconsistentRSHAgainstRSH(snd, fst, reportProgress))
  }

}
