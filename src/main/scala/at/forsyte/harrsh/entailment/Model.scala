package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.Var

/**
  * Created by jens on 2/24/17.
  */
case class Model(stack : Map[Var, Loc], heap : Map[Loc,Seq[Loc]]) {

  assert(stack.keySet.min > 0) // Only non-null free variables are in the stack
  assert(!heap.keySet.contains(0)) // Null is not allocated
  assert(stack.values.toSet subsetOf (heap.keySet ++ heap.values.flatten ++ Set(0)))

}

object Model {
  val empty: Model = Model(Map(), Map())
}
