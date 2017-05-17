package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog.{NullPtr, PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PtrNEq, PureAtom, SymbolicHeap}

/**
  * Created by jkatelaa on 5/16/17.
  */
object ConsistencyCheck {

  def isConsistent(sh : SymbolicHeap) : Boolean = {
    val closure = Closure.ofSetOfAtoms(symbolicHeapToEqualityConstraints(sh))
    closure.isConsistent
  }

  /**
    * Returns the set of all pure constraints immediately implied by sh, meaning both the explicit pure constraints and
    * the inequalities implied by pointer allocation (not congruence-closed).
    * @param sh Symbolic heap whose constraints are computed
    * @return Set of constraints (not congruence-closed)
    */
  def symbolicHeapToEqualityConstraints(sh : SymbolicHeap) : Set[PureAtom] = {
    val qfree : SymbolicHeap = SymbolicHeap.dropQuantifiers(sh)
    val alloc = qfree.pointers map (_.fromAsVar)
    qfree.pure.toSet ++ allocationInfoToConsistencyConstraints(alloc)
  }

  def allocationInfoToConsistencyConstraints(alloc : Seq[Var]) : Iterable[PureAtom] = {
    val allocNotNull : Seq[PureAtom] = alloc map (v => PtrNEq(PtrVar(v), NullPtr()))
    val allocNotEqual : Seq[PureAtom] = for {
      i <- 0 until alloc.size - 1
      j <- i+1 until alloc.size
    } yield PtrNEq(PtrVar(alloc(i)), PtrVar(alloc(j)))
    allocNotNull ++ allocNotEqual
  }

}
