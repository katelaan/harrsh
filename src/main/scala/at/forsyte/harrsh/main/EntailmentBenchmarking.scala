package at.forsyte.harrsh.main

import java.util
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.results.{AverageTimeResult, RunResult}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{Options, OptionsBuilder, TimeValue}

import scala.collection.JavaConverters._
import scala.sys.process._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.{Duration,SECONDS}

sealed trait ToolOutput
case object Valid extends ToolOutput
case object Invalid extends ToolOutput
case class Error(toolOutput: String) extends ToolOutput
case object Unknown extends ToolOutput

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class SlideBenchmarking {

  // Subset of benchmarks in Table 1 on which the respective tool didn't fail
  // Slide
  @Param(Array("examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/leaf-tree_greater-ptree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs"))
  var file: String = ""

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

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class SongbirdBenchmarking {

  // Subset of benchmarks in Table 1 on which the respective tool didn't fail
  // Songbird
  @Param(Array("examples/entailment/tacas2019/acyc-tll_tll.hrs", "examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/tll-classes_tll.hrs", "examples/entailment/tacas2019/tll_acyc-tll.hrs"))
  var file: String = ""

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
    EntailmentBatchMode.runBenchmark(file, suppressOutput = true)
  }

}

class EntailmentBenchmarking {
  // Don't delete this class, otherwise JMH will crash.
}
object EntailmentBenchmarking {

  //val bms = Array("examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs", "examples/entailment/tacas2019/even-sll_sll.hrs", "examples/entailment/tacas2019/greater-ptree_leaf-tree.hrs", "examples/entailment/tacas2019/leaf-tree_greater-ptree.hrs", "examples/entailment/tacas2019/sll_odd-sll.hrs", "examples/entailment/tacas2019/small-ptree_leaf-tree.hrs")
  val bms = Array("examples/entailment/tacas2019/tll-classes_tll.hrs", "examples/entailment/tacas2019/dll_backward_forward.hrs", "examples/entailment/tacas2019/dll_forward_backward.hrs")

  val secs = (i: Int) => TimeValue.seconds(i)
  val WarmupTime = secs(1)
  val IterationTime = secs(2)
  val Timeout = Duration(1, SECONDS)
  val WarmupIterations = 0
  val MeasurementIterations = 1

  val Harrsh = "at.forsyte.harrsh.main.HarrshBenchmarking.benchmarkHarrsh"
  val Songbird = "at.forsyte.harrsh.main.SongbirdBenchmarking.benchmarkSongbird"
  val Slide = "at.forsyte.harrsh.main.SlideBenchmarking.benchmarkSlide"
  val AllTools = Set(Harrsh, Songbird, Slide)

  case class ToolBenchResult(tool: String, file: String, time: Double) {
    assert(AllTools.contains(tool))
  }

  case class BenchResult(file: String, timeByTool: Map[String, Double])

  private def runBenchmarkTillTimeout(opt: Options): Option[ToolBenchResult] = {
    val f: Future[util.Collection[RunResult]] = Future {
      new Runner(opt).run()
    }

    try {
      val res = Await.result(f, Timeout)
      Some(processJmhResults(res))
    } catch {
      case e : TimeoutException =>
        println("Timeout")
        None
    }
  }

  private def processJmhResults(results: util.Collection[RunResult]): ToolBenchResult = {
    (for {
      res <- results.asScala
      tool = res.getParams.getBenchmark
      file = res.getParams.getParam("file")
      avg: AverageTimeResult = res.getAggregatedResult.getPrimaryResult.asInstanceOf[AverageTimeResult]
      score = avg.getScore
      _ = println(s"Finished Benchmark: $tool / $file / $score")
    } yield ToolBenchResult(tool, file, score)).head
  }


  private def runBenchmarkFile(file: String): BenchResult = {
    val toolClasses = Seq(classOf[HarrshBenchmarking], classOf[SongbirdBenchmarking], classOf[SlideBenchmarking])
    val byTool = for {
      tool <- toolClasses
      resBeforeTimeout <- runBenchmarkOnTool(file, tool.getSimpleName)
    } yield resBeforeTimeout
    BenchResult(file, byTool.map(tbr => (tbr.tool, tbr.time)).toMap)
  }

  private def runBenchmarkOnTool(file: String, clazz: String): Option[ToolBenchResult] = {
    val opt: Options = new OptionsBuilder()
      .include(clazz)
      .param("file", file)
      .forks(1)
      .threads(1)
      .warmupTime(WarmupTime)
      .measurementTime(IterationTime)
      //.timeout(Timeout)
      .warmupIterations(WarmupIterations)
      .measurementIterations(MeasurementIterations)
      .build()
//    val results = new Runner(opt).run()
//    (for {
//      res <- results.asScala
//      tool = res.getParams.getBenchmark
//      // res.getParams.getParam("file")
//      avg: AverageTimeResult = res.getAggregatedResult.getPrimaryResult.asInstanceOf[AverageTimeResult]
//      score = avg.getScore
//      _ = println(s"Finished Benchmark: $tool / $file / $score")
//    } yield ToolBenchResult(tool, file, score)).head
    runBenchmarkTillTimeout(opt)
  }

  def main(args: Array[String]): Unit = {
    val resultsByFile = bms map runBenchmarkFile
    //val resultsByFile = aggregateResults(resultsByTool)
    println(resultsByFile.mkString("\n"))
  }
}
