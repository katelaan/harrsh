package at.forsyte.harrsh.entailment.learning

import at.forsyte.harrsh.entailment.SymbolicHeapPartition
import at.forsyte.harrsh.main._
import at.forsyte.harrsh.pure.ConsistencyCheck
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PointsTo, PureAtom}
import at.forsyte.harrsh.util.Combinators

/**
  * Created by jkatelaa on 5/25/17.
  */
trait CloseResultUnderSymmetries extends SymmetryHandler with HarrshLogging {

  override def componentDescriptions : Seq[String] = "symmetry post-processing" +: super.componentDescriptions

  // FIXME Is removing exactly the emp partition a suitable filter here? I suspect that we actually have to filter out all partititons with empty spatial part
  override def keepPartition(spatial : Set[PointsTo], pure : Set[PureAtom]) = spatial.nonEmpty || pure.nonEmpty

  override def symmetryInProcessing(part : SymbolicHeapPartition) : Seq[SymbolicHeapPartition] = Seq(part)

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
          renamedEntry = entry.renameRepParamsTo(renaming)
          if renamedEntry.reps.forall(ConsistencyCheck.isConsistent)
        } yield renamedEntry
    }

    // Problems:
    // - Null-closure / multiple use of parameters can result in sink state!
    // - The null problem persists even if we don't allow null parameters at the top-level, since the rules can still introduce null pointers
    // - Closure can yield redundant states!
    // - Emp-handling?

    // Idea: If there isn't an original class that also makes the entailment true, we add a new class;
    // otherwise we add the renamed representatives to the class
    // This takes care of symmetries?!
    // Is it possible that we do not detect partial overlap in case of symmetrical SIDs? I guess it might be?

    obs.copy(entries = newCandidateEntries)
  }

  private def parameterRenamings(numFV : Int) : Set[Seq[Var]] = {
    Combinators.allSeqsOfLength(numFV, Var.mkAllVars(0 to numFV).toSet)
  }
}
