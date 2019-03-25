package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.{GlobalConfig, IOConfig}
import GlobalConfig.params

case class EntailmentConfig(
                             performInitialSatCheck: Boolean,
                             withPatternMatchingStage: Boolean,
                             useUnionSolver: Boolean,
                             computeSidsForEachSideOfEntailment: Boolean,
                             computeSccsForTopLevelFormulas: Boolean,
                             io: IOConfig
                           ) {

}

object EntailmentConfig {

  def fromGlobalConfig(): EntailmentConfig = {
    EntailmentConfig(
      performInitialSatCheck = GlobalConfig.getBoolean(params.StartEntailmentWithSatCheck),
      withPatternMatchingStage = GlobalConfig.getBoolean(params.WithPatternMatchingStage),
      useUnionSolver = GlobalConfig.getBoolean(params.UseUnionSolver),
      computeSidsForEachSideOfEntailment = GlobalConfig.getBoolean(params.ComputePerSideSids),
      computeSccsForTopLevelFormulas = GlobalConfig.getBoolean(params.ComputeSccs),
      io = IOConfig.fromGlobalConfig()
    )
  }

}
