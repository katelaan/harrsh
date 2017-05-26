package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment.SymbolicHeapPartition
import at.forsyte.harrsh.main._

/**
  * Created by jkatelaa on 5/25/17.
  */
trait LearningStrategy extends LearningComponent with HarrshLogging {

  def iterationPostprocessing(obs : ObservationTable) : ObservationTable

  def checkPartition(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int) : ObservationTable

}
