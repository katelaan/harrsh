package at.forsyte.harrsh.pure

import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.{PureAtom, SymbolicHeap}

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
  private def symbolicHeapToEqualityConstraints(sh : SymbolicHeap) : Set[PureAtom] = {
    // TODO Why drop the quantifiers? The closure computation should work just as well for bound variables
    //val qfree : SymbolicHeap = SymbolicHeap.dropQuantifiers(sh)
    val alloc = sh.pointers map (_.from)
    sh.pure.toSet ++ allocationInfoToConsistencyConstraints(alloc)
  }

  def symbolicHeapToEqualityConstraintsQuantified(sh : SymbolicHeap) : Set[PureAtom] = {
    val alloc = sh.pointers map (_.from)
    sh.pure.toSet ++ allocationInfoToConsistencyConstraints(alloc)
  }

  def allocationInfoToConsistencyConstraints(alloc : Seq[Var]) : Iterable[PureAtom] = {
    val allocNotNull : Seq[PureAtom] = alloc map (_ =/= NullConst)
    val allocNotEqual : Seq[PureAtom] = for {
      i <- 0 until alloc.size - 1
      j <- i+1 until alloc.size
    } yield alloc(i) =/= alloc(j)
    allocNotNull ++ allocNotEqual
  }

}
