package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.{GlobalConfig, IOConfig}
import GlobalConfig.params

case class EntailmentConfig(
                             performInitialSatCheck: Boolean,
                             patternMatchingLevel: Int,
                             useUnionSolver: Boolean,
                             computeSidsForEachSideOfEntailment: Boolean,
                             computeSccsForTopLevelFormulas: Boolean,
                             io: IOConfig
                           ) {
  if (patternMatchingLevel < 0 || patternMatchingLevel > 3)
    throw new IllegalArgumentException(s"Unknown pattern-matching level $patternMatchingLevel")
}

object EntailmentConfig {

  def fromGlobalConfig(): EntailmentConfig = {
    EntailmentConfig(
      performInitialSatCheck = GlobalConfig.getBoolean(params.StartEntailmentWithSatCheck),
      patternMatchingLevel = GlobalConfig.getInt(params.PatternMatchingLevel),
      useUnionSolver = GlobalConfig.getBoolean(params.UseUnionSolver),
      computeSidsForEachSideOfEntailment = GlobalConfig.getBoolean(params.ComputePerSideSids),
      computeSccsForTopLevelFormulas = GlobalConfig.getBoolean(params.ComputeSccs),
      io = IOConfig.fromGlobalConfig()
    )
  }

}
