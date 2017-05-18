package at.forsyte.harrsh.pure

import at.forsyte.harrsh.entailment.{GreedyUnfoldingModelChecker, ReducedEntailment}
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

/**
  * Created by jens on 5/3/17.
  */
object ReducedHeapEquivalence {

  def apply(fst : SymbolicHeap, snd : SymbolicHeap, reportProgress : Boolean = false) : Boolean = {
    // FIXME This only works for garbage-free symbolic heaps due to reliance on pointer unification for garbage-free heaps; might want to generalize!
    // FIXME Implement dedicated heap equivalence test without going through bi-entailment, which is extremely expensive for highly non-determined heaps
    fst == snd || (ReducedEntailment.checkPossiblyInconsistentRSHAgainstRSH(fst, snd, reportProgress) && ReducedEntailment.checkPossiblyInconsistentRSHAgainstRSH(snd, fst, reportProgress))
  }

}
