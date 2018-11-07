package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentInstance
import at.forsyte.harrsh.main.ExecutionMode.EntailmentBatch
import at.forsyte.harrsh.parsers.EntailmentParsers
import at.forsyte.harrsh.util.{IOUtils, StringUtils}
import at.forsyte.harrsh.util.StringUtils.{AlignLeft, AlignRight}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object EntailmentBatchMode {

  val PathToDefaultEntailmentBenchmarks = "examples/entailment"

  def main(args: Array[String]): Unit = {
    runAllEntailmentsInPath(PathToDefaultEntailmentBenchmarks, EntailmentBatch.defaultTimeout)
  }

  def runAllEntailmentsInPath(path: String, timeout: Duration): Unit = {
    val files = IOUtils.allFilesRecursively(path).sorted
    val results = (for {
      file <- files
      if !file.toString.contains("todo")
    } yield runBenchmarkWithTimeout(file.toString, timeout)).toList

    reportAnalysisTimes(results)
    reportFailures(results)
    reportTimeouts(results)
  }

  private def reportAnalysisTimes(results: Seq[EntailmentResult]) = {
    val headings = Seq("Benchmark", "Computed Result", "Time (ms)", "Timeout?", "Error?")
    val minColLengths = Seq(20, 15, 10, 10, 10)
    val alignment = Seq(AlignLeft, AlignRight, AlignRight, AlignRight, AlignLeft)
    val entries = results map {
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

  private def reportFailures(results: Seq[EntailmentResult]) = {
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

  private def reportTimeouts(results: Seq[EntailmentResult]) = {
    val timeouts = results filter (_.timeout)

    if (timeouts.nonEmpty) {
      println("Entailment checks on the following files timed out:")
      println(timeouts.map{
        res => s" - ${res.file}"
      }.mkString("\n"))
    }
  }

  case class EntailmentResult(file: String, computedResult: Option[Boolean], time: Option[Long], timeout: Boolean, failureMsg: Option[String])

  def runBenchmarkWithTimeout(file: String, timeout: Duration): EntailmentResult = {
    val startTime = System.currentTimeMillis()

    val f: Future[(Option[String], Option[Boolean])] = Future {
      runBenchmark(file)
    }

    try {
      val (maybeFailureMsg, maybeResult) = Await.result(f, timeout)
      val endTime = System.currentTimeMillis()
      val timeInMs = endTime - startTime
      println("Finished in " + timeInMs + "ms")
      EntailmentResult(file, maybeResult, Some(timeInMs), timeout = false, maybeFailureMsg)
    } catch {
      case e : TimeoutException =>
        println("Aborting entailment check after reaching timeout (" + timeout + ")")
        EntailmentResult(file, None, None, timeout = true, None)
    }
  }

  def runBenchmark(file: String): (Option[String], Option[Boolean]) = {
    val fileContent = IOUtils.readFile(file.toString)
    Try {
      println(s"Checking $file..."); EntailmentParsers.parse(fileContent, computeSeparateSidsForEachSide = true)
    } match {
      case Failure(exception) => (Some(s"Exception during parsing: ${exception.getMessage}"), None)
      case Success(maybeInstance) =>
        maybeInstance match {
          case Some(instance) =>
            val res = runEntailmentInstance(instance, descriptionOfInstance = file.toString)
            (res._1, res._2)
          case None =>
            (Some("Parse error"), None)
        }
    }
  }

  private def runEntailmentInstance(instance: EntailmentInstance, descriptionOfInstance: String): (Option[String], Option[Boolean], Option[Boolean]) = {
    Try {
      EntailmentChecker.check(descriptionOfInstance, instance, reportProgress = false, exportToLatex = false)
    } match {
      case Failure(exception) => (Some(s"Exception during entailment check: ${exception.getMessage}"), None, None)
      case Success((result, asExpected)) =>
        if (asExpected.getOrElse(true))
          (None, Some(result), asExpected)
        else
          (Some("Unexpected result"), Some(result), asExpected)
    }
  }


}
