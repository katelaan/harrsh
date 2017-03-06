package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.heapautomata.DecisionProcedures.AnalysisResult
import at.forsyte.harrsh.main.{Config, TaskConfig}
import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.util.IOUtils

/**
  * Created by jens on 2/24/17.
  */
object EntailmentPreprocessing {



  def wellDeterminize(sid : SID) : SID = {

    val refined = DecisionProcedures.decideInstance(sid, RunUnsat().getAutomaton(sid.maximumArity), Config.PreprocessingTimeout, verbose = false, reportProgress = false) match {
      case AnalysisResult(true, _, false) =>
        // The set of unsatisfiable unfoldings is empty, no timeout => All unfoldings satisfiable => no refinement necessary
        sid
      case r : AnalysisResult =>
        if (r.timedOut) {
          IOUtils.printWarningToConsole("Unsatisfiability check timed out, will perform SAT-refinement.")
        }
        println("Will well-determinize SID...")
        RefinementAlgorithms.refineSID(sid, RunSat().getAutomaton(sid.maximumArity), Config.PreprocessingTimeout, reportProgress = true) match {
          case Some((rsid,_)) => rsid
          case None =>
            IOUtils.printWarningToConsole("Refinement by SAT timed out, will continue with original SID. If the original SID has unsatisfiable unfoldings, this may be unsound.")
            sid
        }
    }

    determinize(refined)
  }

  def determinize(sid : SID) : SID = ???

}
