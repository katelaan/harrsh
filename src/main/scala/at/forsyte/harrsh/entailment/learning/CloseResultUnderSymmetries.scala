package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.pure.ConsistencyCheck
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 5/25/17.
  */
trait CloseResultUnderSymmetries extends SymmetryHandler with HarrshLogging {

  override def componentDescriptions : Seq[String] = "symmetry post-processing" +: super.componentDescriptions

  override def symmetryInProcessing(rshs : Seq[SymbolicHeap]) : Seq[SymbolicHeap] = rshs

  override def symmetryPostProcessing(obs : ObservationTable) : ObservationTable = {
    logger.warn("Symmetry postprocessing not yet implemented")
    obs
  }

  def closeUnderParameterRenaming(obs : ObservationTable) : ObservationTable = {

    logger.debug("Closing under parameter renamings")

    val newCandidateEntries = obs.entries flatMap {
      entry =>
        for {
          renaming <- parameterRenamings(entry.numFV)
          if renaming != Var.mkAllVars(1 to entry.numFV) // TODO There's a smarter way to achieve this...
          renamedEntry = entry.renameRepParamsTo(renaming)
          if renamedEntry.reps.forall(ConsistencyCheck.isConsistent)
        } yield renamedEntry
    }

    ???

  }

  private def parameterRenamings(numFV : Int) : Set[Seq[Var]] = {
    Combinators.allSeqsOfLength(numFV, Var.mkAllVars(0 to numFV).toSet)
  }
}
