package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.seplog.{Renaming, Var}
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/19/17.
  * Idea: An equivalence class is characterized by the ways its members can be extended to P-unfoldings.
  * Each ECD object represents one such extension, corresponding to the entailment (ext * I)[I/rep] |= P x.
  * Contract: rep.renameVars(repParamInstantiation) * ext |= P x
  *
  * TODO Use dummy call instead of renaming to record the way that the parts should be recombined?
  */
case class SymbolicHeapPartition(rep : SymbolicHeap, ext : SymbolicHeap, repParamInstantiation : Renaming) {
  def repFV = rep.numFV

  def isCombinableWith(that : SymbolicHeapPartition) = repFV == that.repFV

  def combine(that: SymbolicHeapPartition) : (SymbolicHeap, SymbolicHeap) = {
    assert(repFV == that.repFV)
    (SymbolicHeap.mergeHeaps(rep.renameVars(that.repParamInstantiation), that.ext, that.repParamInstantiation.codomain),
      SymbolicHeap.mergeHeaps(that.rep.renameVars(repParamInstantiation), ext, repParamInstantiation.codomain))
  }

  //lazy val recombined = SymbolicHeap.mergeHeaps(rep.renameVarsWithAdditionalQuantification(repParamInstantiation), ext, sharedVars = ext.boundVars)
  // TODO After changes to the semantics of rename vars, we should not need the variant any more, right?
  // Do not rename any quantified variables, they are all shared between the two parts of the partition!
  lazy val recombined = SymbolicHeap.mergeHeaps(rep.renameVars(repParamInstantiation), ext, sharedVars = ext.boundVars.toSet)

  def simplify : SymbolicHeapPartition = copy(
    // Note: Can only simplify rep, because the equalities of the ext can be necessary in recombining into an unfolding
    // The ones in rep cannot be, because all shared bound vars have been replaced by free vars
    rep = EqualityBasedSimplifications.removeExplicitlyRedundantBoundVars(rep))

  def shortString : String = "<<" + rep + repParamInstantiation + ">> * <<" + ext + ">> @ " + repFV

  override def toString = "PARTITION_" + repFV + "(rep = " + rep + repParamInstantiation + ", ext = " + ext + ", unf = " + recombined + ")"

}

object SymbolicHeapPartition {

  def apply(rep : SymbolicHeap, ext : SymbolicHeap) : SymbolicHeapPartition = {
    val repWithExtPoints = unbindShared(rep, ext)
    SymbolicHeapPartition(repWithExtPoints._1, ext, Renaming.fromMap(repWithExtPoints._2))
  }

  /**
    * Converts the bound variables in rshToModify thar are shared with sharedWith into additional free variables and returns the result
    * @param rshToModify Heap in which variables will be renamed
    * @param sharedWith Heap with (potentially) shared variables that rshToModify's vars are compared against
    * @return rshToModify with renamed vars + map witnessing the renaming
    */
  private def unbindShared(rshToModify : SymbolicHeap, sharedWith : SymbolicHeap) : (SymbolicHeap,Map[Var,Var]) = {
    def unbindAll(vars : Seq[Var], sh : SymbolicHeap, map : Map[Var,Var]) : (SymbolicHeap,Map[Var,Var]) = if (vars.isEmpty) {
      (sh,map)
    } else {
      val unusedVars = sh.freeVars.toSet -- sh.usedFreeVars
      // FIXME: If there is more than one unused FV, we should actually generate one partition per possible instantiation choice. We don't do that, yet; So as soon as that happens in practice, the following assertion will fail
      assert(unusedVars.size <= 1)
      val minimalUnusedFV = if (unusedVars.isEmpty) sh.numFV+1 else unusedVars.min
      val nextSH = sh.instantiateBoundVars(Seq((vars.head, minimalUnusedFV)), closeGaps = false)
      unbindAll(vars.tail, nextSH, map + (minimalUnusedFV -> vars.head))
    }

    val sharedVars = rshToModify.boundVars.toSet intersect sharedWith.boundVars.toSet
    // FIXME Actually should consider all ways to order the FVs? See also the comment above
    unbindAll(sharedVars.toSeq, rshToModify, Map.empty)
  }

}