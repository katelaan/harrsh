package at.forsyte.harrsh.modelchecking

import at.forsyte.harrsh.seplog.inductive.{SidLike, SymbolicHeap}

/**
  * Created by jkatelaa on 3/3/17.
  */
trait SymbolicHeapModelChecker {

  def isModel(model: Model, formula : SymbolicHeap, sid: SidLike): Boolean

  final def isModel(model : Model, sid : SidLike) : Boolean = {
    isModel(model, sid.callToStartPred, sid)
  }

}
