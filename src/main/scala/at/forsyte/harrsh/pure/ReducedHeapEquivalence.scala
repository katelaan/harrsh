package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 5/3/17.
  */
object ReducedHeapEquivalence {

  def apply(fst : SymbolicHeap, snd : SymbolicHeap) : Boolean = {
    // FIXME Proper equivalence check
    fst == snd
  }

}
