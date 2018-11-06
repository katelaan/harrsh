package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentInstance
import at.forsyte.harrsh.main.ExecutionMode.EntailmentBatch
import at.forsyte.harrsh.parsers.EntailmentParsers
import at.forsyte.harrsh.util.{IOUtils, StringUtils}

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
    val files = IOUtils.allFilesRecursively(path)
    val results = (for {
      file <- files
      if !file.toString.contains("todo")
    } yield runBenchmarkWithTimeout(file.toString, timeout)).toList

    reportAnalysisTimes(results)
    reportFailures(results)
    reportTimeouts(results)
  }

  private def reportAnalysisTimes(results: Seq[EntailmentResult]) = {
    val headings = Seq("Benchmark", "Analysis time (ms)", "Timeout?", "Error?")
    val minColLengths = Seq(20, 20, 10, 10)
    val entries = results map {
      res => Seq(res.file, ""+res.time.getOrElse("-"),  ""+res.timeout, res.failureMsg.getOrElse("-"))
    }
    println(StringUtils.toTable(headings, minColLengths, entries))
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

  case class EntailmentResult(file: String, time: Option[Long], timeout: Boolean, failureMsg: Option[String])

  def runBenchmarkWithTimeout(file: String, timeout: Duration): EntailmentResult = {
    val startTime = System.currentTimeMillis()

    val f: Future[Option[String]] = Future {
      runBenchmark(file)
    }

    try {
      val maybeFailureMsg = Await.result(f, timeout)
      val endTime = System.currentTimeMillis()
      val timeInMs = endTime - startTime
      println("Finished in " + timeInMs + "ms")
      EntailmentResult(file, Some(timeInMs), timeout = false, maybeFailureMsg)
    } catch {
      case e : TimeoutException =>
        println("Aborting entailment check after reaching timeout (" + timeout + ")")
        EntailmentResult(file, None, timeout = true, None)
    }
  }

  def runBenchmark(file: String): Option[String] = {
    val fileContent = IOUtils.readFile(file.toString)
    Try {
      println(s"Checking $file..."); EntailmentParsers.parse(fileContent, computeSeparateSidsForEachSide = true)
    } match {
      case Failure(exception) => Some(s"Exception during parsing: ${exception.getMessage}")
      case Success(maybeInstance) =>
        maybeInstance match {
          case Some(instance) =>
            runEntailmentInstance(instance, descriptionOfInstance = file.toString)
          case None =>
            Some("Parse error")
        }
    }
  }

  private def runEntailmentInstance(instance: EntailmentInstance, descriptionOfInstance: String) = {
    Try {
      !EntailmentChecker.check(descriptionOfInstance, instance, reportProgress = false, exportToLatex = false)
    } match {
      case Failure(exception) => Some(s"Exception during entailment check: ${exception.getMessage}")
      case Success(unexpectedResult) => if (unexpectedResult) Some("Unexpected result") else None
    }
  }


}
