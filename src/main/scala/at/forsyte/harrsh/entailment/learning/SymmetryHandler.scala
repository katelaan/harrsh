package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

/**
  * Created by jkatelaa on 5/25/17.
  */
trait SymmetryHandler extends LearningComponent {

  def symmetryInProcessing(rshs : Seq[SymbolicHeap]) : Seq[SymbolicHeap]

  def symmetryPostProcessing(obs : ObservationTable) : ObservationTable

}
