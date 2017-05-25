package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 5/25/17.
  */
trait CloseUnfoldingsUnderSymmetries extends SymmetryHandler {

  override def symmetryInProcessing(rshs : Seq[SymbolicHeap]) : Seq[SymbolicHeap] = ???

  override def symmetryPostProcessing(obs : ObservationTable) : ObservationTable = obs

}
