package at.forsyte.harrsh.main

import java.util
import java.util.concurrent.TimeUnit

import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentStats
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.{AverageTimeResult, RunResult}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{Options, OptionsBuilder, TimeValue}

import scala.collection.JavaConverters._
import scala.sys.process._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.{Duration, SECONDS}
import BenchmarkingConfig._
import at.forsyte.harrsh.util.{IOUtils, StringUtils}

object BenchmarkingConfig {

  // Tools
  val Harrsh = "at.forsyte.harrsh.main.HarrshBenchmarking.benchmarkHarrsh"
  val Songbird = "at.forsyte.harrsh.main.SongbirdBenchmarking.benchmarkSongbird"
  val Slide = "at.forsyte.harrsh.main.SlideBenchmarking.benchmarkSlide"
  val AllTools = Set(Harrsh, Songbird, Slide)

  // Paths
  val ResultTexFile = "complete-results.tex"
  val ResultTxtFile = "results.txt"
  val BenchmarkPath = "examples/entailment"

  // Benchmark times
  val ShortTest = false
  val Timeout = Duration(if (ShortTest) 3 else 180, SECONDS)
  private val secs = (i: Int) => TimeValue.seconds(i)
  val WarmupTime = secs(if (ShortTest) 0 else 20)
  val IterationTime = secs(if (ShortTest) 1 else 100)
  val WarmupIterations = 1
  val MeasurementIterations = 1
}

sealed trait ToolOutput {
  val statusString = this match {
    case Valid(stats) => "true"
    case Invalid(stats) => "false"
    case ToolError(toolOutput) => "?"
    case Unknown => "?"
    case ToolTimeout => "?"
  }

  val isSuccess = this match {
    case Valid(_) => true
    case Invalid(_) => true
    case ToolError(_) => false
    case Unknown => false
    case ToolTimeout => false
  }

  val maybeStats = this match {
    case Valid(stats) => stats
    case Invalid(stats) => stats
    case ToolError(toolOutput) => None
    case Unknown => None
    case ToolTimeout => None
  }

  val result: Option[Boolean] = this match {
    case Valid(_) => Some(true)
    case Invalid(_) => Some(false)
    case ToolError(toolOutput) => None
    case Unknown => None
    case ToolTimeout => None
  }
}

case class Valid(stats: Option[EntailmentStats]) extends ToolOutput
case class Invalid(stats: Option[EntailmentStats]) extends ToolOutput
case class ToolError(toolOutput: String) extends ToolOutput
case object Unknown extends ToolOutput
case object ToolTimeout extends ToolOutput

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class SlideBenchmarking {

  // Subset of benchmarks in Table 1 on which the respective tool didn't fail
  // Slide
  @Param(Array("examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/leaf-tree_greater-ptree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs"))
  var file: String = ""

  @Benchmark
  def benchmarkSlide(): String = {
    SlideBenchmarking.runSlide(file)
    //slideResult(file).toString
  }

}
object SlideBenchmarking {

  var p: Option[Process] = None

  def runSlide(file: String): String = {
    val slideFileLeft = file + ".lhs.pred"
    val slideFileRight = file + ".rhs.pred"
    //println(sbFile)
    //val call = s"python3 ./slide/entailment.py $slideFileLeft $slideFileRight"
    //call.!!
    val call = Seq("python3", "./slide/entailment.py", slideFileLeft, slideFileRight)
    val allOutput = new StringBuilder()
    p = Some(call run ProcessLogger(line => allOutput.appendAll(line + '\n'), line => allOutput.appendAll(line + '\n')))
    p map (_.exitValue())
    allOutput.mkString
  }

  def slideResult(file: String): ToolOutput = {
    try {
      val output = runSlide(file)
      //.lineStream.last
      val lines = output.split("\n")
      val verdict = if (lines.exists(_.startsWith("VALID"))) Valid(None)
      else if (lines.exists(_.startsWith("INVALID"))) Invalid(None)
      else ToolError(output)
      println("Slide Verdict: " + verdict)
      verdict
    } catch {
      case _:java.lang.RuntimeException =>
        println("Slide crashed on " + file)
        ToolError("Crash")
    }
  }
}

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class SongbirdBenchmarking {

  // Subset of benchmarks in Table 1 on which the respective tool didn't fail
  // Songbird
  @Param(Array("examples/entailment/tacas2019/acyc-tll_tll.hrs", "examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/tll-classes_tll.hrs", "examples/entailment/tacas2019/tll_acyc-tll.hrs"))
  var file: String = ""

  @Benchmark
  def benchmarkSongbird(): String = {
    SongbirdBenchmarking.runSongbird(file)
    //songbirdResult(file).toString
  }

}

object SongbirdBenchmarking {

  var p: Option[Process] = None

  def runSongbird(file: String): String = {
    val sbFile = file + ".sb"
    //println(sbFile)
    //val call = s"./songbird.native $sbFile"
    //call.!!
    val call = Seq("./songbird.native", sbFile)
    val allOutput = new StringBuilder()
    p = Some(call run ProcessLogger(line => allOutput.appendAll(line + '\n'), line => allOutput.appendAll(line + '\n')))
    p map (_.exitValue())
    allOutput.mkString
  }

  def songbirdResult(file: String): ToolOutput = {
    val output = runSongbird(file)
    val verdict = output.trim match {
      case "sat" => Invalid(None)
      case "unsat" => Valid(None)
      case "unknown" => Unknown
      case msg => ToolError(msg)
    }
    println("Songbird Verdict: " + verdict)
    verdict
  }

}

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class HarrshBenchmarking {

  // Subset of benchmarks in Table 1 on which the respective tool didn't fail
  // Harrsh
  @Param(Array("examples/entailment/tacas2019/acyc-tll_tll.hrs", "examples/entailment/tacas2019/acyclic-sll_sll.hrs", "examples/entailment/tacas2019/almost-linear-treep_treep.hrs", "examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/external_equality.hrs", "examples/entailment/tacas2019/external_null_missing.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/leaf-tree_greater-ptree.hrs", "examples/entailment/tacas2019/leaf-tree_ptree.hrs", "examples/entailment/tacas2019/leftmost_leaf_to_root.hrs", "examples/entailment/tacas2019/odd-or-even-sll_sll.hrs", "examples/entailment/tacas2019/odd-sll_sll.hrs", "examples/entailment/tacas2019/ptree_leaf-tree.hrs", "examples/entailment/tacas2019/sll_acyclic-sll.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/tll-classes_tll.hrs", "examples/entailment/tacas2019/tll_acyc-tll.hrs", "examples/entailment/tacas2019/treep_almost-linear-treep.hrs", "examples/entailment/k-grids/dlgrid-left-right.hrs"))
  var file: String = ""

  @Benchmark
  def benchmarkHarrsh(): EntailmentBatchMode.BenchmarkTrace = {
    HarrshBenchmarking.runHarrsh(file)
  }

}

object HarrshBenchmarking {

  def runHarrsh(file: String): EntailmentBatchMode.BenchmarkTrace = {
    EntailmentBatchMode.runBenchmark(file, suppressOutput = true)
  }

  def harrshResult(file: String): ToolOutput = {
    val trace = runHarrsh(file)
    trace.result match {
      case Some(value) =>
        if (value) Valid(trace.stats) else Invalid(trace.stats)
      case None => ToolError("")
    }
  }
}

object ToolRunner {

  def apply(file: String, run: String => ToolOutput, cleanup: () => Unit): ToolOutput = {

    val f: Future[ToolOutput] = Future {
      run(file)
    }

    try {
      Await.result(f, BenchmarkingConfig.Timeout)
    } catch {
      case e : TimeoutException =>
        println(s"Timeout reached on $file => Will execute cleanup routine.")
        ToolTimeout
    } finally {
      cleanup()
    }
  }

}

class EntailmentBenchmarking {
  // Don't delete this class, otherwise JMH will crash.
}
object EntailmentBenchmarking {

  case class ToolBenchResult(tool: String, time: Double) {
    assert(AllTools.contains(tool))
  }

  case class BenchResult(file: String, timeByTool: Map[String, Double])

  private def runJmhBenchmark(opt: Options): ToolBenchResult = {
    val res = new Runner(opt).run()
    processJmhResults(res)
  }

  private def processJmhResults(results: util.Collection[RunResult]): ToolBenchResult = {
    (for {
      res <- results.asScala
      tool = res.getParams.getBenchmark
      file = res.getParams.getParam("file")
      avg: AverageTimeResult = res.getAggregatedResult.getPrimaryResult.asInstanceOf[AverageTimeResult]
      score = avg.getScore
      _ = println(s"Finished Benchmark: $tool / $file / $score")
    } yield ToolBenchResult(tool, score)).head
  }


  private def runJmhOnBenchmarkFile(file: String): BenchResult = {
    val toolClasses = Seq(classOf[HarrshBenchmarking], classOf[SongbirdBenchmarking], classOf[SlideBenchmarking])
    val byTool = for {
      tool <- toolClasses
    } yield runJmhBenchmarkForTool(file, tool.getSimpleName)
    BenchResult(file, byTool.map(tbr => (tbr.tool, tbr.time)).toMap)
  }

  case class ToolSpec(name: String, clazz: Class[_], cmd: String => ToolOutput, cleanup: () => Unit)

  val toolSpecs = Seq(
    ToolSpec("HRS",
      classOf[HarrshBenchmarking],
      HarrshBenchmarking.harrshResult, () => ()),
    ToolSpec("SB", classOf[SongbirdBenchmarking],
      SongbirdBenchmarking.songbirdResult,
      () => SongbirdBenchmarking.p.foreach(_.destroy())),
    ToolSpec("SLD", classOf[SlideBenchmarking],
      SlideBenchmarking.slideResult,
      () => SlideBenchmarking.p.foreach(_.destroy()))
  )

  private def resultDeviationMarker(hrs: ToolTableEntry, other: ToolTableEntry) = {
    (for {
      hrsRes <- hrs.result
      otherRes <- other.result
    } yield if (hrsRes != otherRes) " (*)" else "").getOrElse("")
  }

  case class TableEntry(file: String, tools: Seq[ToolTableEntry]) {
    def toColumnSeq: Seq[String] = {
      val emptyEntry = (toolName:String) => ToolTableEntry(toolName, Unknown, None)
      val hrs = tools.find(_.toolName == "HRS").getOrElse(emptyEntry("HRS"))
      val sb = tools.find(_.toolName == "SB").getOrElse(emptyEntry("SB"))
      val sld = tools.find(_.toolName == "SLD").getOrElse(emptyEntry("SLD"))
      val sbSuffix = resultDeviationMarker(hrs, sb)
      val sldSuffix = resultDeviationMarker(hrs, sld)

      val query = file.split("/").last.replace("_","\\_")
      Seq(query, hrs.output.statusString, hrs.timeString, sb.timeString + sbSuffix, sld.timeString + sldSuffix) ++ hrs.statsColumns
    }

    def errorMessages: Seq[String] = Seq() ++ tools.flatMap(_.errorMsg)
  }

  case class ToolTableEntry(toolName: String, output: ToolOutput, jmhRes: Option[ToolBenchResult]) {

    private val jmhTime = jmhRes match {
      case Some(tbr) => tbr.time.toInt.toString
      case None => "?"
    }

    val timeString: String = output match {
      case Valid(_) => jmhTime
      case Invalid(_) => jmhTime
      case ToolError(toolOutput) => "(X)"
      case Unknown => "(U)"
      case ToolTimeout => "TO"
    }

    def statsColumns: Seq[String] = output.maybeStats match {
      case Some(stats) => Seq(stats.numProfiles, stats.totalNumDecomps, stats.totalNumContexts) map (_.toString)
      case None => Seq("?", "?", "?")
    }

    def errorMsg: Option[String] = output match {
      case ToolError(toolOutput) => Some(s"$toolName failed. Diagnostic info (if any): $toolOutput")
      case _ => None
    }

    val result = output.result
  }

  private def runBenchmarkFile(file: String): TableEntry = {
    TableEntry(file, toolSpecs map (runToolOnBenchmarkFile(file, _)))
  }

  private def runToolOnBenchmarkFile(file: String, toolSpec: ToolSpec): ToolTableEntry = {
    // First see if the tool fails/times out
    val output = ToolRunner(file, toolSpec.cmd, toolSpec.cleanup)
    if (output.isSuccess) {
      // Run JMH only in case of success
      println(s"${toolSpec.name} solved $file before timeout => Use JMH to benchmark.")
      ToolTableEntry(toolSpec.name, output, Some(runJmhBenchmarkForTool(file, toolSpec.clazz.getSimpleName)))
    } else {
      println(s"${toolSpec.name} timed out or crashed on $file => Skip JMH")
      ToolTableEntry(toolSpec.name, output, None)
    }
  }

  private def runJmhBenchmarkForTool(file: String, toolClass: String): ToolBenchResult = {
    val opt: Options = new OptionsBuilder()
      .include(toolClass)
      .param("file", file)
      .forks(1)
      .threads(1)
      .warmupTime(WarmupTime)
      .measurementTime(IterationTime)
      .warmupIterations(WarmupIterations)
      .measurementIterations(MeasurementIterations)
      .build()

    runJmhBenchmark(opt)
  }

  private def exportResultsToLatex(results: Seq[TableEntry]): Unit = {
    val headings = Seq("File", "Status", "HRS", "SB", "SLD", "\\#P", "\\#D", "\\#C")
    val entries = results map (_.toColumnSeq)
    val bulletPoints = for {
      res <- results
      msg <- res.errorMessages
      bulletPoint = res.file + ": " + msg
    } yield bulletPoint.replace("_","\\_").replaceAllLiterally("<", "\\textless{}").replaceAllLiterally(">", "\\textgreater{}")
    MainIO.writeLatexFile(ResultTexFile, headings, entries, bulletPoints)
  }

  private def resultsToPlainTextTable(results: Seq[TableEntry]): String = {
    val headings = Seq("File", "Status", "HRS", "SB", "SLD", "\\#P", "\\#D", "\\#C")
    val entries = results map (_.toColumnSeq)
    val table = StringUtils.toTable(StringUtils.defaultTableConfigForHeadings(headings), entries)

    val bulletPoints = for {
      res <- results
      msg <- res.errorMessages
    } yield " - " + res.file + ": " + msg

    table ++ "\n\n" ++ bulletPoints.mkString("\n")
  }

  def runJmhBenchmarskForHarrsh(fileWithBenchmarks: String): Unit = {
    val bms = IOUtils.readFile(fileWithBenchmarks).split("\n")
    var resultsByFile: List[TableEntry] = Nil
    for (bm <- bms) {
      resultsByFile = resultsByFile :+ runBenchmarkFile(bm) //TableEntry(bm, Seq(runToolOnBenchmarkFile(bm, toolSpecs.head)))
      val resultTable = resultsToPlainTextTable(resultsByFile)
      println(s"FINISHED $bm -- Results so far:")
      println(resultTable)
      println("Writing intermediate results to: " + ResultTxtFile)
      IOUtils.writeFile(ResultTxtFile, resultTable)
    }
    println(s"FINISHED ALL BENCHMARKS. See $ResultTxtFile for full result table")
  }

  def main(args: Array[String]): Unit = {
    val bms = EntailmentBatchMode.allHarrshEntailmentFilesInPath(BenchmarkPath)
    //val bms = Seq("examples/entailment/tacas2019/2-grid.hrs", "examples/entailment/tacas2019/acyclic-sll_sll.hrs", "examples/entailment/tacas2019/almost-linear-treep_treep.hrs", "examples/entailment/tacas2019/dlgrid.hrs", "examples/entailment/tacas2019/dlgrid-left-right.hrs", "examples/entailment/various/list-segments-different-order.hrs")
    //val bms = Seq("examples/entailment/tacas2019/tll-classes_tll.hrs")

    var resultsByFile: List[TableEntry] = Nil
    for (bm <- bms) {
      resultsByFile = resultsByFile :+ runBenchmarkFile(bm)
      println(s"FINISHED $bm -- Results so far:")
      println(resultsByFile.mkString("\n"))
      println("Writing table to: " + ResultTexFile)
      exportResultsToLatex(resultsByFile)
    }
    println("FINISHED ALL BENCHMARKS")
  }
}
