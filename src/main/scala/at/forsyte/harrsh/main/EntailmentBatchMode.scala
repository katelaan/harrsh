package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.entailment.EntailmentChecker.{EntailmentInstance, EntailmentStats}
import at.forsyte.harrsh.parsers.EntailmentParsers
import at.forsyte.harrsh.util.{IOUtils, StringUtils}
import at.forsyte.harrsh.util.StringUtils.{AlignLeft, AlignRight}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object EntailmentBatchMode {

  val PathToSlcompEntailmentBenchmarks = "bench/qf_shid_entl"
  val PathToDefaultEntailmentBenchmarks = "examples/entailment"
  val ResultTexFile = "entailment-stats.tex"

  case class EntailmentResult(file: String, computedResult: Option[Boolean], time: Option[Long], timeout: Boolean, failureMsg: Option[String], stats: Option[EntailmentStats])

  def main(args: Array[String]): Unit = {
    parseAllEntailmentsInPath(PathToSlcompEntailmentBenchmarks, computeSidsForEachSideOfEntailment = false)
    //runAllEntailmentsInPath(PathToDefaultEntailmentBenchmarks, EntailmentBatch.defaultTimeout)
  }

  def parseAllEntailmentsInPath(path: String, computeSidsForEachSideOfEntailment: Boolean): Unit = {
    val files = IOUtils.allFilesRecursively(path).sorted
    val results = for {
      file <- files
      filename = file.toString
      if !filename.contains("todo")
      if !filename.endsWith("info")
    } yield {
      println(s"Will try to parse $filename")
      EntailmentParsers.fileToEntailmentInstance(filename, computeSidsForEachSideOfEntailment)
    }
    println("All parse results:")
    println(results.mkString("\n ******* \n"))
  }

  def runAllEntailmentsInPath(path: String, timeout: Duration): Unit = {
    val files = IOUtils.allFilesRecursively(path).sorted
    val results = (for {
      file <- files
      if !file.toString.contains("todo")
    } yield runBenchmarkWithTimeout(file.toString, timeout)).toList

    reportAnalysisTimes(results.map(_._2))
    reportFailures(results.map(_._2))
    reportTimeouts(results.map(_._2))
    println(s"Will export results to $ResultTexFile...")
    exportResultsToLatex(results)
    println("Done.")
  }

  private def reportAnalysisTimes(bms: Seq[EntailmentResult]): Unit = {
    val headings = Seq("Benchmark", "Computed Result", "Time (ms)", "Timeout?", "Error?")
    val minColLengths = Seq(20, 15, 10, 10, 10)
    val alignment = Seq(AlignLeft, AlignRight, AlignRight, AlignRight, AlignLeft)
    val entries = bms map {
      res => Seq(
        res.file,
        ""+res.computedResult.getOrElse("-"),
        ""+res.time.getOrElse("-"),
        if (res.timeout) "yes" else "no",
        res.failureMsg.getOrElse("-"))
    }
    val config = StringUtils.TableConfig(headings, minColLengths, alignment)
    println(StringUtils.toTable(config, entries))
  }

  private def exportResultsToLatex(results: Seq[(Option[EntailmentInstance], EntailmentResult)]): Unit = {
    //val headings = Seq("Benchmark", "Computed Result", "Time (ms)", "Timeout?", "Error?")
    val headings = Seq("Benchmark", "Status", "Time (ms)", "\\#preds", "\\#profiles", "\\#decomps", "\\#contexts")
    val fromStats = (maybeStats: Option[EntailmentStats], f: EntailmentStats => Any) => maybeStats.map(f).map(""+_).getOrElse("-")
    val desc = (maybeEI: Option[EntailmentInstance], res: EntailmentResult) => maybeEI match {
      case Some(ei) => "$" + ei.lhsCall + " \\models " + ei.rhsCall + "$"
      case None => res.file.split("/").last.replace("_","\\_")
    }
    val entries = results map {
      case (maybeEI, res) => Seq(
        desc(maybeEI, res),
        ""+res.computedResult.getOrElse("-"),
        ""+res.time.getOrElse("-"),
        //if (res.timeout) "yes" else "no",
        //res.failureMsg.getOrElse("-"))
        fromStats(res.stats, _.numExploredPreds),
        fromStats(res.stats, _.numProfiles),
        fromStats(res.stats, _.totalNumDecomps),
        fromStats(res.stats, _.totalNumContexts)
      )
    }
    MainIO.writeLatexFile(ResultTexFile, headings, entries)
  }

  private def reportFailures(results: Seq[EntailmentResult]): Unit = {
    val failures = results filter (_.failureMsg.nonEmpty)

    if (failures.isEmpty) {
      println("All entailment checks returned the expected results.")
    } else {
      println("Entailment checks on the following files went wrong:")
      println(failures.map{
        res => s" - ${res.file}: ${res.failureMsg.getOrElse("")}"
      }.mkString("\n"))
    }
  }

  private def reportTimeouts(results: Seq[EntailmentResult]): Unit = {
    val timeouts = results filter (_.timeout)

    if (timeouts.nonEmpty) {
      println("Entailment checks on the following files timed out:")
      println(timeouts.map{
        res => s" - ${res.file}"
      }.mkString("\n"))
    }
  }

  case class BenchmarkTrace(instance: Option[EntailmentInstance], result: Option[Boolean], stats: Option[EntailmentStats], errorMsg: Option[String])

  def runBenchmarkWithTimeout(file: String, timeout: Duration): (Option[EntailmentInstance],EntailmentResult) = {
    val startTime = System.currentTimeMillis()

    val f: Future[BenchmarkTrace] = Future {
      runBenchmark(file)
    }

    try {
      val BenchmarkTrace(maybeInstance, maybeResult, maybeStats, maybeFailureMsg) = Await.result(f, timeout)
      val endTime = System.currentTimeMillis()
      val timeInMs = endTime - startTime
      println("Finished in " + timeInMs + "ms")
      (maybeInstance, EntailmentResult(file, maybeResult, Some(timeInMs), timeout = false, maybeFailureMsg, maybeStats))
    } catch {
      case e : TimeoutException =>
        println("Aborting entailment check after reaching timeout (" + timeout + ")")
        (None, EntailmentResult(file, None, None, timeout = true, None, None))
    }
  }

  def runBenchmark(file: String): BenchmarkTrace = {
    Try {
      println(s"Checking $file..."); EntailmentParsers.fileToEntailmentInstance(file, computeSidsForEachSideOfEntailment = true)
    } match {
      case Failure(exception) => BenchmarkTrace(None, None, None, Some(s"Exception during parsing: ${exception.getMessage}"))
      case Success(maybeInstance) =>
        maybeInstance match {
          case Some(instance) =>
            val res = runEntailmentInstance(instance, descriptionOfInstance = file.toString)
            BenchmarkTrace(Some(instance), res._2, res._4, res._1)
          case None =>
            BenchmarkTrace(None, None, None, Some("Parse error"))
        }
    }
  }

  private def runEntailmentInstance(instance: EntailmentInstance, descriptionOfInstance: String): (Option[String], Option[Boolean], Option[Boolean], Option[EntailmentStats]) = {
    Try {
      EntailmentChecker.check(descriptionOfInstance, instance, reportProgress = false, exportToLatex = false)
    } match {
      case Failure(exception) => (Some(s"Exception during entailment check: ${exception.getMessage}"), None, None, None)
      case Success((result, asExpected, maybeStats)) =>
        if (asExpected.getOrElse(true))
          (None, Some(result), asExpected, maybeStats)
        else
          (Some("Unexpected result"), Some(result), asExpected, maybeStats)
    }
  }


}
