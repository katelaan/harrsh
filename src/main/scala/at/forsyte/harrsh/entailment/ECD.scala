package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{MapBasedRenaming, Renaming, Var}
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jens on 3/19/17.
  * Contract: rep.renameVars(repRenaming) * ext.renameVars(extRenaming) |= P x
  */
case class ECD(rep : SymbolicHeap, ext : SymbolicHeap, repRenaming : Renaming, extRenaming : Renaming) {
  def repFV = rep.numFV

  def isCombinableWith(that : ECD) = repFV == that.repFV

  def combine(that: ECD) : (SymbolicHeap, SymbolicHeap) = {
    (SymbolicHeap.combineHeaps(rep, that.ext), SymbolicHeap.combineHeaps(that.rep, ext))
    //assert(repFV == that.repFV)
    //(SymbolicHeap.combineHeaps(rep.renameVars(that.repRenaming), that.ext), SymbolicHeap.combineHeaps(that.rep, ext))
  }

  lazy val recombined = SymbolicHeap.combineHeapsWithoutAlphaConversion(rep.renameVarsWithAdditionalQuantification(repRenaming), ext.renameVarsWithAdditionalQuantification(extRenaming))

  override def toString = "ECD_" + repFV + "(rep = " + rep + repRenaming + ", ext = " + ext + extRenaming + ", unf = " + recombined + ")"

}

object ECD {

  def apply(rep : SymbolicHeap, ext : SymbolicHeap) : ECD = {
    val repWithExtPoints = unbindShared(rep, ext)
    val extWithExtPoints = unbindShared(ext, rep)
    ECD(repWithExtPoints._1, extWithExtPoints._1, MapBasedRenaming(repWithExtPoints._2), MapBasedRenaming(extWithExtPoints._2))
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
      val nextSH = sh.instantiateBoundVar(vars.head, sh.numFV+1)
      unbindAll(vars.tail, nextSH, map + (sh.numFV+1 -> vars.head))
    }

    val sharedVars = rshToModify.boundVars.toSet intersect sharedWith.boundVars.toSet
    // FIXME Actually should consider all ways to order the FVs? See also the comment above
    unbindAll(sharedVars.toSeq, rshToModify, Map.empty)
  }

}