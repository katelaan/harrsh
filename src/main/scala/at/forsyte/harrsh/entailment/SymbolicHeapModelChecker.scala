package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.PtrVar
import at.forsyte.harrsh.seplog.inductive.{PredCall, SID, SymbolicHeap}

/**
  * Created by jkatelaa on 3/3/17.
  */
trait SymbolicHeapModelChecker {

  def isModel(model: Model, formula : SymbolicHeap, sid: SID): Boolean

  final def isModel(model : Model, sid : SID) : Boolean = {
    isModel(model, sid.callToStartPred, sid)
  }



}
