package at.forsyte.harrsh.refinement

import at.forsyte.harrsh.main.GlobalConfig.params
import at.forsyte.harrsh.main.{GlobalConfig, ProblemStatus, SatQuery}

import scala.concurrent.duration.Duration

object SatChecker {

  def apply(bm: SatQuery): Boolean = {
    val verbose = GlobalConfig.getBoolean(params.Verbose)
    val sid = bm.toIntegratedSid
    !DecisionProcedures.decideInstance(
      sid,
      RunSat.getAutomaton,
      None,
      incrementalFromNumCalls = Some(GlobalConfig.getInt(params.SatCheckingIncrementalFromNumCalls)),
      skipSinksAsSources = true,
      verbose,
      reportProgress = verbose)
  }

  def apply(bm: SatQuery, timeout: Duration): (ProblemStatus, Long) = {
    val verbose = GlobalConfig.getBoolean(params.Verbose)
    val sid = bm.toIntegratedSid
    val res: DecisionProcedures.AnalysisResult = DecisionProcedures.decideInstance(
      sid,
      RunSat.getAutomaton,
      timeout,
      None,
      incrementalFromNumCalls = Some(GlobalConfig.getInt(params.SatCheckingIncrementalFromNumCalls)),
      skipSinksAsSources = true,
      verbose,
      reportProgress = verbose)
    val resStatus = if (res.timedOut) {
      ProblemStatus.Unknown
    } else {
      if (res.isEmpty) ProblemStatus.Incorrect else ProblemStatus.Correct
    }
    (resStatus, res.analysisTime)
  }

}
