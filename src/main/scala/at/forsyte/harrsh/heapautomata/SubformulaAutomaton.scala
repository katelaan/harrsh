package at.forsyte.harrsh.heapautomata

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.PtrExpr
import at.forsyte.harrsh.seplog.Var._
import at.forsyte.harrsh.seplog.inductive.{PureAtom, SymbolicHeap}

/**
  * Created by jens on 2/26/17.
  *
  * TODO Main difficulty: Quantified variables may be named differently
  */
class SubformulaAutomaton(spatial : Set[PtrExpr], pure: Set[PureAtom]) extends HeapAutomaton with HarrshLogging {

  /**
   * Pointers already matched, pure atoms already matched, has there been a contradiction?
   */
  override type State = (Set[PtrExpr], Set[PureAtom], Boolean)

  override final def doesAlphabetContain(lab: SymbolicHeap): Boolean = true

  override final def implementsTargetComputation: Boolean = true


  override val states: Set[State] = ???

  override def isFinal(s: State): Boolean = !s._3 && ???

  private def spatialEquality(lhs : Set[PtrExpr], rhs : Set[PtrExpr]) : Boolean = {
    // TODO Fix this -- need to take care of bound vars of different names
    lhs == rhs
  }

  private def pureEntailment(lhs : Set[PureAtom], rhs : Set[PureAtom]) : Boolean = ???
}
