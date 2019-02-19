package at.forsyte.harrsh.main

import java.io.File

import at.forsyte.harrsh.parsers.QueryParser.FileExtensions

import scala.concurrent.duration.{Duration, SECONDS}
import at.forsyte.harrsh.parsers.slcomp
import at.forsyte.harrsh.refinement.{DecisionProcedures, RefinementInstance, RunSat}
import at.forsyte.harrsh.util.IOUtils

import scala.collection.mutable.ListBuffer
import scala.util.Try

object SlCompMode {

  val DIRS = List("bench/qf_shls_sat", "bench/qf_shid_sat")
  //val DIRS = List("bench/test")
  val DEFAULT_TIMEOUT_IN_SECS = 120
  val BATCH_TIMEOUT_IN_SECS = 5
  val SKIP_WORSTCASE_INSTANCES = false

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      return
    }

    args(0) match {
      case "say" =>
        println(args(1) + '!')
      case "parse" =>
        if (args(1) == "all")
          parseAllBenchmarks()
        else {
          val bm = parseBenchmark(args(1))
          println(s"Constructed the following benchmark:\n$bm")
        }
      case "run" =>
        val incrementalFromNumCalls = try {
          args.lift(2).map(Integer.parseInt)
        } catch {
          case _:Throwable => None
        }
        //println(incrementalFromNumCalls)
        run(args(1), incrementalFromNumCalls)
      case "check" =>
        val verbose = try {
          args(2) == "verbose"
        } catch {
          case _: Throwable => false
        }

        if (args(1) == "all")
          checkAll(verbose = verbose)
       else
          check(args(1), verbose = verbose)
    }
  }

  def run(file: String, incrementalFromNumCalls: Option[Int] = None, timeoutInSecs: Int = DEFAULT_TIMEOUT_IN_SECS) = {
    val res = for {
      bm <- parseBenchmark(file)
      status <- execute(bm, timeoutInSecs)._1.toBoolean
    } yield (bm.isInstanceOf[EntailmentQuery], status)
    val output = res match {
      case None => "unknown"
      case Some((isEntailment, true)) => if (isEntailment) "unsat" else "sat"
      case Some((isEntailment, false)) => if (isEntailment) "sat" else "unsat"
    }
    println(output)
  }

  private case class BenchmarkResult(status: ProblemStatus, deviation: Boolean, time: Long)

  private def checkAll(timeoutInSecs: Int = BATCH_TIMEOUT_IN_SECS, verbose: Boolean = false, printBm: Boolean = false): Unit = {
    val stats:ListBuffer[(String,ProblemStatus,Boolean,Long)] = new ListBuffer
    for (bench <- allSlcompBenchs().sortBy(_.toString)) {
      val res = check(bench.toString, verbose)
      println(s"${bench.toString}: Computed result: ${res.status}, as expected: ${res.deviation}, used ${res.time}ms")
      stats.append((bench.toString, res.status, res.deviation, res.time))
    }

    println("FINISHED BENCHMARK SUITE")
  }

  private def check(file: String, verbose: Boolean = false): BenchmarkResult = {
    println(s"Will check $file...")
    parseBenchmark(file) match {
      case None =>
        println(s"Couldn't parse $file")
        BenchmarkResult(ProblemStatus.Unknown, deviation = true, 0)
      case Some(bm) =>
        checkBenchmark(bm, verbose)
    }
  }

  private def checkBenchmark(bm: Query, verbose: Boolean): BenchmarkResult = {
    if (verbose) {
      println(s"Benchmark: $bm")
      println(s"Input for refinement: $bm")
      println(s"Expected result: ${bm.status}")
    }
    val (res, time) = execute(bm, verbose = verbose)
    if (verbose) println(s"Done. Result: $res")
    val deviation = if (bm.status != ProblemStatus.Unknown && bm.status != ProblemStatus.Unknown && bm.status != res) {
      println("UNEXPECTED RESULT")
      false
    } else {
      true
    }
    BenchmarkResult(res, deviation, time)
  }

  private def execute(bm: Query, timeoutInSecs: Int = DEFAULT_TIMEOUT_IN_SECS, verbose: Boolean = false, incrementalFromNumCalls: Option[Int] = None): (ProblemStatus, Long) = {
    bm match {
      case q: SatQuery => executeSatQuery(q, timeoutInSecs, verbose, incrementalFromNumCalls)
      case q: EntailmentQuery => executeEntailmentQuery(q, timeoutInSecs, verbose, incrementalFromNumCalls)
      case _ =>
        println("Input parsed to wrong query type.")
        (ProblemStatus.Unknown, 0)
    }
  }

  private def executeSatQuery(bm: SatQuery, timeoutInSecs: Int, verbose: Boolean, incrementalFromNumCalls: Option[Int]): (ProblemStatus, Long) = {
    val sid = bm.toIntegratedSid
    val timeout = Duration(timeoutInSecs, SECONDS)
    incrementalFromNumCalls match {
      case Some(value) => RefinementInstance.IncrementalFromNumCalls = value
      case None => // Nothing to do
    }
    val res: DecisionProcedures.AnalysisResult = DecisionProcedures.decideInstance(
      sid,
      RunSat.getAutomaton,
      timeout,
      skipSinksAsSources = true,
      verbose = verbose,
      reportProgress = verbose)
    val resStatus = if (res.timedOut) {
      ProblemStatus.Unknown
    } else {
      if (res.isEmpty) ProblemStatus.Incorrect else ProblemStatus.Correct
    }
    (resStatus, res.analysisTime)
  }

  private def executeEntailmentQuery(bm: EntailmentQuery, timeoutInSecs: Int, verbose: Boolean, incrementalFromNumCalls: Option[Int]): (ProblemStatus, Long) = {
    println("No support for entailment checking.")
    (ProblemStatus.Unknown, 0)
  }

  private def parseBenchmark(file: String): Option[Query] = {
    slcomp.parseFileToQuery(file)
  }

  private def parseAllBenchmarks(): Unit = {
    var parsed = 0
    var failed = 0
    var failedNames: List[String] = Nil
    for (bench <- allSlcompBenchs().sortBy(_.toString)) {
      println(s"Try parsing $bench...")
      val maybeParsed = Try {
        parseBenchmark(bench.toString).get
      }.toOption
      maybeParsed match {
        case Some(query) =>
          println(s"Constructed the following benchmark:\n$query")
          parsed += 1
      case None =>
        failed += 1
        failedNames = failedNames :+ bench.toString
      }
    }

    println(s"Successfully parsed $parsed/${parsed+failed} benchmarks.")
    if (failedNames.nonEmpty) {
      println("Failed benchmarks:")
      for (f <- failedNames) println(s" - $f")
    }
  }

  private def allSlcompBenchs(): List[File] = {
    for {
      dir <- DIRS
      file <- IOUtils.getListOfFiles(dir)
      if file.getName.endsWith(FileExtensions.SlComp) && file.getName != "logic.smt2"
      if !SKIP_WORSTCASE_INSTANCES || !file.getName.startsWith("succ-")
    } yield file
  }

}
