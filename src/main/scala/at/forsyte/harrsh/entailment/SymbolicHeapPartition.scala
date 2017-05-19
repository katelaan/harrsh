package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.EqualityBasedSimplifications
import at.forsyte.harrsh.seplog.{PtrExpr, Var}
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}
import at.forsyte.harrsh.util.IOUtils

/**
  * Created by jens on 3/19/17.
  * A SymbolicHeapPartition represents a partition of a symbolic heap phi such that (ext * I)[I/rep] == phi.
  * The variable extPredicateCall represents the dummy predicate call I
  */
case class SymbolicHeapPartition(rep : SymbolicHeap, ext : SymbolicHeap, extPredCall : PredCall) {

  assert(rep.isReduced)
  assert(ext.isReduced)

  // FIXME This assertion should be falsified later when we allow gaps in the free vars of representatives; see also comment in companion object
  if (rep.numFV > extPredCall.args.size) {
    IOUtils.printWarningToConsole("Rep " + rep + " has " + rep.numFV + " fvs, but call is " + extPredCall)
    assert(rep.numFV <= extPredCall.args.size)
  }

  def repFV = rep.numFV

  def isCombinableWith(that : SymbolicHeapPartition) = repFV == that.repFV

  lazy val recombined = ext.copy(predCalls = Seq(extPredCall)).replaceCall(extPredCall, rep)

  def simplify : SymbolicHeapPartition = {
    // Note: Can only simplify rep, because the equalities of the ext can be necessary in recombining into an unfolding
    // The ones in rep cannot be, because all shared bound vars have been replaced by free vars
    copy(rep = EqualityBasedSimplifications.removeExplicitlyRedundantBoundVars(rep))
  }

  def shortString : String = "<<" + rep + ">> * <<" + SymbolicHeapPartition.combinedString(ext, extPredCall) + ">> @ " + repFV

  override def toString = "PARTITION_" + repFV + "(rep = " + rep + ", ext = " + ext + " * " + extPredCall + ", unf = " + recombined + ")"

}

object SymbolicHeapPartition extends HarrshLogging {

  def combinedString(pair : (SymbolicHeap, PredCall)) : String = pair._1 + " * " + pair._2

  private val partitionDummyPredicateName : String = "I"

  def partitionsFromUnbindingSharedVars(rep : SymbolicHeap, ext : SymbolicHeap) : Set[SymbolicHeapPartition] = {
    val repsWithExtPoints = unbindShared(rep, ext)
    repsWithExtPoints map {
      case (renamedRepresentative, renamingMap) => SymbolicHeapPartition(renamedRepresentative, ext, predCallFromRenamingMap(renamedRepresentative.numFV, renamingMap))
    }
  }

  private def predCallFromRenamingMap(arity : Int, map : Map[Var,Var]) : PredCall = {
    assert(map.keys.forall(Var.isFV))
    assert(map.values.forall(Var.isBound))

    val freeVars = 1 to arity
    val callArgs = freeVars map (fv => PtrExpr(if (map.isDefinedAt(fv)) map(fv) else fv))

    PredCall(partitionDummyPredicateName, callArgs)
  }

  /**
    * Converts the bound variables in rshToModify thar are shared with sharedWith into additional free variables and returns the result
    * @param rshToModify Heap in which variables will be renamed
    * @param sharedWith Heap with (potentially) shared variables that rshToModify's vars are compared against
    * @return rshToModify with renamed vars + map witnessing the renaming (mapping free vars to the bound vars they replaced)
    */
  private def unbindShared(rshToModify : SymbolicHeap, sharedWith : SymbolicHeap) : Set[(SymbolicHeap,Map[Var,Var])] = {

    def unbindAll(vars : Seq[Var], sh : SymbolicHeap, map : Map[Var,Var]) : Set[(SymbolicHeap,Map[Var,Var])] = if (vars.isEmpty) {
      Set((sh,map))
    } else {
      val unusedVars = sh.freeVars.toSet -- sh.usedFreeVars
      // FIXME Should we also consider using a fresh FV even if there is an unused one? If the free variable bound is larger than the number of vars we need here, gaps in the set of FVs used should be possible...
      val instantiations = if (unusedVars.isEmpty) Set(sh.numFV + 1) else unusedVars

      instantiations flatMap {
        unusedFV =>
          val nextSH = sh.instantiateBoundVars(Seq((vars.head, unusedFV)), closeGaps = false)
          unbindAll(vars.tail, nextSH, map + (unusedFV -> vars.head))
      }
    }

    val sharedVars = rshToModify.boundVars.toSet intersect sharedWith.boundVars.toSet
    val res = unbindAll(sharedVars.toSeq, rshToModify, Map.empty)

    if (res.size <= 1) {
      logger.trace("There is a unique way to make a partition out of " + rshToModify + " and " + sharedWith)
    } else {
      logger.debug("There are " + res.size + " ways to make a partition out of " + rshToModify + " and " + sharedWith)
    }
    res
  }

}