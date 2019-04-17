package at.forsyte.harrsh.main

import GlobalConfig.params

case class IOConfig(
                   debug: Boolean,
                   verbose: Boolean,
                   reportProgress : Boolean,
                   printResult: Boolean,
                   exportToLatex: Boolean
) {

}

object IOConfig {

  def fromGlobalConfig(): IOConfig = {
    IOConfig(
      debug = GlobalConfig.getBoolean(params.Debug),
      verbose = GlobalConfig.getBoolean(params.Verbose),
      reportProgress = GlobalConfig.getBoolean(params.ReportProgress),
      printResult = GlobalConfig.getBoolean(params.PrintResult),
      exportToLatex = GlobalConfig.getBoolean(params.ToLatex)
    )
  }

  val EntailmentResultLatexFile = "entailment.tex"

}