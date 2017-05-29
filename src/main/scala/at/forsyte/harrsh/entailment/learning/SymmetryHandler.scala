package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment.SymbolicHeapPartition
import at.forsyte.harrsh.seplog.inductive.{PointsTo, PureAtom}
/**
  * Created by jkatelaa on 5/25/17.
  */
trait SymmetryHandler extends LearningComponent {

  def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) : Boolean

  def symmetryInProcessing(part : SymbolicHeapPartition) : Seq[SymbolicHeapPartition]

  def symmetryPostProcessing(obs : ObservationTable) : ObservationTable

}
