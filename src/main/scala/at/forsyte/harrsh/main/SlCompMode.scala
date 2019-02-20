package at.forsyte.harrsh.main

import java.io.File

import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.parsers.QueryParser.FileExtensions

import scala.concurrent.duration.{Duration, SECONDS}
import at.forsyte.harrsh.parsers.slcomp
import at.forsyte.harrsh.refinement.{DecisionProcedures, RefinementInstance, RunSat}
import at.forsyte.harrsh.util.IOUtils

import scala.collection.mutable.ListBuffer
import scala.util.Try

object SlCompMode {

  object config {
    val ComputePerSideSids = false
    val Timeout = 10
    val IncrementalFromNumCalls = 4
  }

  //val DIRS = List("bench/qf_shls_sat", "bench/qf_shid_sat")
  val DIRS = List("bench/qf_shls_entl", "bench/qf_shid_entl")
  val BATCH_TIMEOUT_IN_SECS = 5
  val SKIP_WORSTCASE_INSTANCES = false

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      return
    }

    val verbose = try {
      args(2) == "verbose"
    } catch {
      case _: Throwable => false
    }

    args(0) match {
      case "say" =>
        println(args(1) + '!')
        for {
          bench <- allSlcompBenchs().sorted
        } println(bench)
      case "parse" =>
        if (args(1) == "all")
          parseAllBenchmarks()
        else {
          val bm = parseBenchmark(args(1))
          println(s"Constructed the following benchmark:\n$bm")
        }
      case "run" =>
        val incrementalFromNumCalls = Try { Integer.parseInt(args(2)) }.toOption.getOrElse(config.IncrementalFromNumCalls)
        run(args(1), incrementalFromNumCalls, config.Timeout)
      case "check" =>
        if (args(1) == "all")
          checkAll(config.Timeout, verbose = verbose)
        else
          check(args(1), verbose = verbose)
      case "batch" =>
        val bmFile = args(1)
        val files = for {
          line <- IOUtils.readFile(bmFile).lines
          trimmed = line.trim
          if trimmed.nonEmpty
          if trimmed(0) != '#'
        } yield trimmed
        checkList(files.toList, config.Timeout, verbose = verbose)
    }
  }

  def run(file: String, incrementalFromNumCalls: Int, timeoutInSecs: Int) = {
    val res = for {
      bm <- parseBenchmark(file)
      status <- execute(bm, timeoutInSecs, incrementalFromNumCalls = Some(incrementalFromNumCalls))._1.toBoolean
    } yield (bm.isInstanceOf[EntailmentQuery], status)
    val output = res match {
      case None => "unknown"
      case Some((isEntailment, true)) => if (isEntailment) "unsat" else "sat"
      case Some((isEntailment, false)) => if (isEntailment) "sat" else "unsat"
    }
    println(output)
  }

  private case class BenchmarkResult(status: ProblemStatus, deviation: Boolean, time: Long)

  private def checkAll(timeoutInSecs: Int, verbose: Boolean = false, printBm: Boolean = false): Unit = {
    checkList(allSlcompBenchs().map(_.toString).sorted, timeoutInSecs, verbose, printBm)
  }

  private def checkList(bms: List[String], timeoutInSecs: Int, verbose: Boolean = false, printBm: Boolean = false): Unit = {
    val stats:ListBuffer[(String,ProblemStatus,Boolean,Long)] = new ListBuffer
    for (bench <- bms) {
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
    val (res, time) = execute(bm, config.Timeout, verbose = verbose)
    if (verbose) println(s"Done. Result: $res")
    val deviation = if (bm.status != ProblemStatus.Unknown && bm.status != ProblemStatus.Unknown && bm.status != res) {
      println("UNEXPECTED RESULT")
      false
    } else {
      true
    }
    BenchmarkResult(res, deviation, time)
  }

  private def execute(bm: Query, timeoutInSecs: Int, verbose: Boolean = false, incrementalFromNumCalls: Option[Int] = None): (ProblemStatus, Long) = {
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
    // TODO: Measure time
    bm.toEntailmentInstance(computeSeparateSidsForEachSide = config.ComputePerSideSids) match {
      case None =>
        println(s"Couldn't convert $bm to entailment instance")
        (ProblemStatus.Unknown, 0)
      case Some(ei) =>
//        val (res, stats) = EntailmentChecker.solve(ei)
//        if (verbose) stats.foreach(println)
//        val status = if (res) ProblemStatus.Correct else ProblemStatus.Incorrect
//        val time = 0
//        (status, time)
        println("Is BTW-SID.")
        (ProblemStatus.Unknown, 0)
    }
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
