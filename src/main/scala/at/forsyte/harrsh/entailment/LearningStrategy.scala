package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentLearningLog.RedEntCheck.ExtensionCompatibilityCheck
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConsistencyCheck

/**
  * Created by jkatelaa on 5/25/17.
  */
trait LearningStrategy extends HarrshLogging {

  def iterationPostprocessing(obs : ObservationTable) : ObservationTable

  def checkPartition(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int) : ObservationTable

}
