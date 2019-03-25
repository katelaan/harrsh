package at.forsyte.harrsh.main

import at.forsyte.harrsh.refinement.AutomatonTask

import scala.collection.mutable
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.Try

object GlobalConfig {

  val BatchExtension: String = "bms"

  object params {
    // General
    val Timeout = "timeout"
    val Verbose = "verbose"
    val Debug = "debug"
    val ReportProgress = "report-progress"
    val PrintResult = "print-result"
    val ToLatex = "export-latex"
    val Property = "prop"
    // Sat Checking
    val SatCheckingIncrementalFromNumCalls = "sat-incremental"
    // Entailment Checking
    val PatternMatchingLevel = "pattern-matching"
    val ComputePerSideSids = "per-side-sid"
    val ComputeSccs = "compute-sccs"
    val StartEntailmentWithSatCheck = "with-sat-check"
    val UseUnionSolver = "union-solver"
    // Model Checking
    val ModelFile = "model"
    // SID Exploration
    val UnfoldingsReduced = "reduced-only"
    val UnfoldingDepth = "unfolding-depth"
    // Batch Mode
    val IsBatchMode = "is-batch-mode"
    val BatchBaseDir = "dir"
    val BatchTimeout = "batch-timeout"
    val BatchCategories = "categories"
    val BatchSkipWorstCase = "skip-succ"
  }

  private val map: mutable.Map[String, Any] = mutable.Map(
    // General
    params.Timeout -> 2400,
    params.Verbose -> false,
    params.Debug -> false,
    params.ReportProgress -> false,
    params.PrintResult -> true,
    params.ToLatex -> false,
    params.Property -> AutomatonTask.keywords.sat,
    // Sat Checking
    params.SatCheckingIncrementalFromNumCalls -> 6,
    // Entailment Checking
    params.StartEntailmentWithSatCheck -> false,
    params.PatternMatchingLevel -> 3,
    params.ComputePerSideSids -> true,
    params.UseUnionSolver -> true,
    params.ComputeSccs -> false,
    // SID Exploration
    params.UnfoldingsReduced -> false,
    params.UnfoldingDepth -> 3,
    // Batch Mode
    params.IsBatchMode -> false,
    params.BatchBaseDir -> "bench",
    params.BatchTimeout -> 5,
    params.BatchCategories -> "all",
    params.BatchSkipWorstCase -> true
  )

  override def toString: String = {
    map.toSeq.sortBy(_._1).map(p => s"  ${p._1} = ${p._2}").mkString("Config(\n", "\n", "\n)")
  }

  def isDefinedAt(key: String): Boolean = map.isDefinedAt(key)

  def set(key: String, value: Any): Unit = map.update(key, value)

  def getBoolean(key: String): Boolean = map(key).asInstanceOf[Boolean]
  def getInt(key: String): Int = map(key).asInstanceOf[Int]
  def getDuration(key: String): Duration = Duration(map(key).asInstanceOf[Int], SECONDS)
  def getString(key: String): String = map(key).asInstanceOf[String]

  def getProp = {
    val res = AutomatonTask.fromString(getString(params.Property))
    if (res.isEmpty) println(s"Could not parse property '${getString(params.Property)}'")
    res
  }

  def getTimeoutForCurrentMode: Duration = {
    getDuration(if (getBoolean(params.IsBatchMode)) params.BatchTimeout else params.Timeout)
  }

  def batchDirs: Seq[String] = {
    val baseDir = getString(params.BatchBaseDir)
    val addPath = (cat: String) => baseDir + '/' + cat
    val all@Seq(qf_shls_sat, qf_shid_sat, qf_shls_entl, qf_shlid_entl, qf_shid_entl) = Seq(
      "qf_shls_sat", "qf_shid_sat", "qf_shls_entl", "qf_shlid_entl", "qf_shid_entl"
    ).map(addPath)
    getString(params.BatchCategories) match {
      case "all" => all
      case "sat" => Seq(qf_shls_sat, qf_shid_sat)
      case "entl" => Seq(qf_shls_entl, qf_shlid_entl, qf_shid_entl)
      case other => other.split(",").map(addPath)
    }
  }

  def addKeyValuePairsToConfig(args: Iterable[String]): Unit = {
    args foreach GlobalConfig.addKeyValuePairToConfig
  }

  def addKeyValuePairToConfig(arg: String): Unit = {
    try {
      val (key, eqSignAndvalue) = arg.span(_ != '=')
      val value = eqSignAndvalue.tail
      if (value == "true" || value == "false") {
        set(key, value == "true")
      } else {
        val asInt = Try {
          Integer.parseInt(value)
        }.toOption
        asInt match {
          case None => set(key, value)
          case Some(i) => set(key, i)
        }
      }
    } catch {
      case e: Throwable => println(s"Couldn't parse $arg as key-value pair: ${e.getMessage}")
    }
  }

}
