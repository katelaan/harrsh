package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment.SymbolicHeapPartition
import at.forsyte.harrsh.pure.ConsistencyCheck
import at.forsyte.harrsh.seplog.{PtrExpr, Var}
import at.forsyte.harrsh.seplog.inductive.{PointsTo, PredCall, PureAtom, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 5/25/17.
  */
trait ClosePartitionsUnderSymmetries extends SymmetryHandler {

  override def componentDescriptions: Seq[String] = "symmetry in-processing" +: super.componentDescriptions

  override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = true

  override def symmetryInProcessing(part: SymbolicHeapPartition): Seq[SymbolicHeapPartition] = {
    // Reorder variables / Substitute variables with null
    val reordered = for {
      permutation <- Combinators.permutations(Var.mkAllVars(1 to part.repFV))
      renamedRep = SymbolicHeap.renameFVs(part.rep, permutation)
      ext = part.ext
      reorderedCall = ClosePartitionsUnderSymmetries.predCallReordering(part.extPredCall, permutation)
    } yield SymbolicHeapPartition(renamedRep, ext, reorderedCall)

    //    for {
    //      rsh <- rshs
    //      renaming <- parameterRenamings(rsh.numFV)
    //      res = SymbolicHeap.renameFVs(rsh, renaming)
    //      if ConsistencyCheck.isConsistent(res)
    //    } yield res
    reordered
  }

  override def symmetryPostProcessing(obs: ObservationTable): ObservationTable = obs

}

object ClosePartitionsUnderSymmetries {

  def predCallReordering(call : PredCall, reordering : Seq[Var]) : PredCall = call match {
    case PredCall(name, args) =>
      def matchArg(k : Int) : PtrExpr = {
        // TODO More efficient implementation of permutation reversal
        // If we have variable x_k (rather than the original x_j) at position j in the reordering,
        // then we use the j-th parameter of the original predicate call at position k in the new predicate call
        // This has the effect of reversing the permutation
        val j = reordering.indexOf(Var(k))
        assert(j >= 0)
        args(j)
      }
      PredCall(name, (1 to reordering.size) map matchArg)
  }

}
