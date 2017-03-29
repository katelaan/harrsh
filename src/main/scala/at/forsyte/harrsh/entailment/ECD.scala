package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{MapBasedRenaming, Renaming, Var}
import at.forsyte.harrsh.seplog.inductive.{DefaultNaming, SymbolicHeap}

/**
  * Created by jens on 3/19/17.
  * Idea: An equivalence class is characterized by the ways its members can be extended to P-unfoldings.
  * Each ECD object represents one such extension, corresponding to the entailment (ext * I)[I/rep] |= P x.
  * Contract: rep.renameVars(repParamInstantiation) * ext |= P x
  */
case class ECD(rep : SymbolicHeap, ext : SymbolicHeap, repParamInstantiation : Renaming) {
  def repFV = rep.numFV

  def isCombinableWith(that : ECD) = repFV == that.repFV

  def combine(that: ECD) : (SymbolicHeap, SymbolicHeap) = {
    assert(repFV == that.repFV)
    (SymbolicHeap.mergeHeaps(rep.renameVars(that.repParamInstantiation), that.ext, that.repParamInstantiation.codomain),
      SymbolicHeap.mergeHeaps(that.rep.renameVars(repParamInstantiation), ext, repParamInstantiation.codomain))
  }

  //lazy val recombined = SymbolicHeap.mergeHeaps(rep.renameVarsWithAdditionalQuantification(repParamInstantiation), ext, sharedVars = ext.boundVars)
  // TODO After changes to the semantics of rename vars, we should not need the variant any more, right?
  // Do not rename any quantified variables, they are all shared between the two parts of the partition!
  lazy val recombined = SymbolicHeap.mergeHeaps(rep.renameVars(repParamInstantiation), ext, sharedVars = ext.boundVars.toSet)

  override def toString = "ECD_" + repFV + "(rep = " + rep + repParamInstantiation + ", ext = " + ext + ", unf = " + recombined + ")"

}

object ECD {

  def apply(rep : SymbolicHeap, ext : SymbolicHeap) : ECD = {
    val repWithExtPoints = unbindShared(rep, ext)
    ECD(repWithExtPoints._1, ext, MapBasedRenaming(repWithExtPoints._2))
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
      val nextSH = sh.instantiateBoundVarWithFV(vars.head, sh.numFV+1)
      unbindAll(vars.tail, nextSH, map + (sh.numFV+1 -> vars.head))
    }

    val sharedVars = rshToModify.boundVars.toSet intersect sharedWith.boundVars.toSet
    // FIXME Actually should consider all ways to order the FVs? See also the comment above
    unbindAll(sharedVars.toSeq, rshToModify, Map.empty)
  }

}