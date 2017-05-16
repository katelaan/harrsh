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

  def allocationInfoToConsistencyConstraints(alloc : Iterable[Var]) : Iterable[PureAtom] = {
    val allocNotNull : Iterable[PureAtom] = alloc map (v => PtrNEq(PtrVar(v), NullPtr()))
    val allocNotEqual : Iterable[PureAtom] = for {
    // TODO: Might be silightly faster not to generate the entire cross product of alloc with itself...
      fst <- alloc
      snd <- alloc
      if fst < snd
    } yield PtrNEq(PtrVar(fst), PtrVar(snd))
    allocNotNull ++ allocNotEqual
  }

}
