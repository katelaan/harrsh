package at.forsyte.harrsh.main

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.AverageTimeResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{Options, OptionsBuilder, TimeValue}

import scala.collection.JavaConverters._
import scala.sys.process._

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
//@BenchmarkMode(Array(Mode.AverageTime, Mode.SampleTime))
@BenchmarkMode(Array(Mode.AverageTime))
//@BenchmarkMode(Array(Mode.SingleShotTime))
class EntailmentBenchmarking {

  sealed trait ToolOutput
  case object Valid extends ToolOutput
  case object Invalid extends ToolOutput
  case class Error(toolOutput: String) extends ToolOutput
  case object Unknown extends ToolOutput

  // Subset of benchmarks in Table 1 on which the respective tools didn't fail. Uncomment one of the lines to check just these via the default jmh CLI call
  // Harrsh
  //@Param(Array("examples/entailment/tacas2019/acyc-tll_tll.hrs", "examples/entailment/tacas2019/acyclic-sll_sll.hrs", "examples/entailment/tacas2019/almost-linear-treep_treep.hrs", "examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/external_equality.hrs", "examples/entailment/tacas2019/external_null_missing.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/leaf-tree_greater-ptree.hrs", "examples/entailment/tacas2019/leaf-tree_ptree.hrs", "examples/entailment/tacas2019/leftmost_leaf_to_root.hrs", "examples/entailment/tacas2019/odd-or-even-sll_sll.hrs", "examples/entailment/tacas2019/odd-sll_sll.hrs", "examples/entailment/tacas2019/ptree_leaf-tree.hrs", "examples/entailment/tacas2019/sll_acyclic-sll.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/tll-classes_tll.hrs", "examples/entailment/tacas2019/tll_acyc-tll.hrs", "examples/entailment/tacas2019/treep_almost-linear-treep.hrs", "examples/entailment/k-grids/dlgrid-left-right.hrs"))
  // Songbird
  //@Param(Array("examples/entailment/tacas2019/acyc-tll_tll.hrs", "examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/tll-classes_tll.hrs", "examples/entailment/tacas2019/tll_acyc-tll.hrs"))
  // Slide
  @Param(Array("examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/leaf-tree_greater-ptree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs"))
  var file: String = ""

  @Benchmark
  def benchmarkHarrsh(): EntailmentBatchMode.BenchmarkTrace = {
    EntailmentBatchMode.runBenchmark(file, suppressOutput = true)
  }

  def runSongbird(file: String): String = {
    val sbFile = file + ".sb"
    //println(sbFile)
    val call = s"./songbird.native $sbFile"
    call.!!
  }

  @Benchmark
  def benchmarkSongbird(): String = {
    //runSongbird(file)
    songbirdResult(file).toString
  }

  def songbirdResult(file: String): ToolOutput = {
    val output = runSongbird(file)
    val verdict = output.trim match {
      case "sat" => Invalid
      case "unsat" => Valid
      case "unknown" => Unknown
      case msg => Error(msg)
    }
    println("Songbird Verdict: " + verdict)
    verdict
  }

  def runSlide(file: String): String = {
    val slideFileLeft = file + ".lhs.pred"
    val slideFileRight = file + ".rhs.pred"
    //println(sbFile)
    val call = s"python3 ./slide/entailment.py $slideFileLeft $slideFileRight"
    call.!!
  }

  def slideResult(file: String): ToolOutput = {
    val output = runSlide(file)//.lineStream.last
    val lines = output.split("\n")
    val verdict = if (lines.exists(_.startsWith("VALID"))) Valid
    else if (lines.exists(_.startsWith("INVALID"))) Invalid
    else Error(output)
    println("Slide Verdict: " + verdict)
    verdict
  }

  @Benchmark
  def benchmarkSlide(): String = {
    //runSlide(file)
    slideResult(file).toString
  }
}

object EntailmentBenchmarking {

  val secs = (i: Int) => TimeValue.seconds(i)
  val WarmupTime = secs(1)
  val IterationTime = secs(2)
  // TODO: Instead handle by future outside JMH!
  //val Timeout = secs(1)
  val WarmupIterations = 0
  val MeasurementIterations = 1

  val Harrsh = "at.forsyte.harrsh.main.EntailmentBenchmarking.benchmarkHarrsh"
  val Songbird = "at.forsyte.harrsh.main.EntailmentBenchmarking.benchmarkSongbird"
  val Slide = "at.forsyte.harrsh.main.EntailmentBenchmarking.benchmarkSlide"
  val AllTools = Set(Harrsh, Songbird, Slide)

  case class ToolBenchResult(tool: String, file: String, time: Double) {
    assert(AllTools.contains(tool))
  }

  case class BenchResult(file: String, timeByTool: Map[String, Double])

  def aggregateResults(results: Iterable[ToolBenchResult]) = {
    results.groupBy(_.file).map{
      case (file, toolResults) => BenchResult(file, toolResults.map(tbr => (tbr.tool, tbr.time)).toMap)
    }
  }

  //val bms = Array("examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/leaf-tree_greater-ptree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs")
  val bms = Array("examples/entailment/tacas2019/tll-classes_tll.hrs", "examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs")

//  def runBenchmarkTillTimeout(opt: Options): Option[Iterable[ToolBenchResult]] = {
//    Some(new Runner(opt).run())
//  }

  private def runBenchmarkFile(file: String): Iterable[ToolBenchResult] = {
    val opt: Options = new OptionsBuilder()
      .include(classOf[EntailmentBenchmarking].getSimpleName)
      .param("file", file)
      .forks(1)
      .threads(1)
      .warmupTime(WarmupTime)
      .measurementTime(IterationTime)
      //.timeout(Timeout)
      .warmupIterations(WarmupIterations)
      .measurementIterations(MeasurementIterations)
      .build()
    val results = new Runner(opt).run()
    for {
      res <- results.asScala
      tool = res.getParams.getBenchmark
      // res.getParams.getParam("file")
      avg: AverageTimeResult = res.getAggregatedResult.getPrimaryResult.asInstanceOf[AverageTimeResult]
      score = avg.getScore
      _ = println(s"Finished Benchmark: $tool / $file / $score")
    } yield ToolBenchResult(tool, file, score)
  }

  def main(args: Array[String]): Unit = {
    val resultsByTool = for {
      bm <- bms
      res <- runBenchmarkFile(bm)
    } yield res
    val resultsByFile = aggregateResults(resultsByTool)
    println(resultsByFile.mkString("\n"))
  }
}
