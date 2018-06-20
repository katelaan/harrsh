package at.forsyte.harrsh.main

import java.io.File

import scala.concurrent.duration.{Duration, SECONDS}
import at.forsyte.harrsh.parsers.slcomp
import at.forsyte.harrsh.refinement.{DecisionProcedures, RunSat}
import at.forsyte.harrsh.seplog.SatBenchmark

import scala.collection.mutable.ListBuffer

object SlCompMode {

  val DIRS = List("bench/qf_shls_sat", "bench/qf_shid_sat")
  val DEFAULT_TIMEOUT_IN_SECS = 60
  val BATCH_TIMEOUT_IN_SECS = 10
  val SKIP_WORSTCASE_INSTANCES = true

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      return
    }

    args(0) match {
      case "parse" =>
        if (args(1) == "all")
          parseAllBenchmarks()
        else {
          val bm = parseBenchmark(args(1))
          println(s"Constructed the following benchmark:\n$bm")
        }
      case "run" =>
        run(args(1))
      case "check" =>
        if (args(1) == "all")
          checkAll()
        else
          check(args(1))
    }
  }

  def run(file: String, timeoutInSecs: Int = DEFAULT_TIMEOUT_IN_SECS) = {
    val bm = parseBenchmark(file)
    val res = execute(bm, timeoutInSecs)
    println(res._1)
  }

  def checkAll(timeoutInSecs: Int = BATCH_TIMEOUT_IN_SECS): Unit = {
    val stats:ListBuffer[(String,SatBenchmark.Status,SatBenchmark.Status,Long)] = new ListBuffer
    for (bench <- allSatBenchs().sortBy(_.toString)) {
      val bm = parseBenchmark(bench.toString)
      val (res,time) = execute(bm, timeoutInSecs)
      println(s"${bench.toString}: Expected ${bm.status}, got $res, used ${time}ms")
      stats.append((bench.toString, bm.status, res, time))
    }

    println("FINISHED BENCHMARK SUITE")
  }

  def check(file: String, verbose: Boolean = false): Boolean = {
    println(s"Will check $file...")
    val bm = parseBenchmark(file)
    if (verbose) {
      println(s"Benchmark: $bm")
      println(s"Input for refinement: ${bm.toIntegratedSid}")
      println(s"Expected result: ${bm.status}")
    }
    val (res,_) = execute(bm, verbose = verbose)
    println(s"Done. Result: $res")
    if (bm.status != SatBenchmark.Unknown && bm.status != SatBenchmark.Unknown && bm.status != res) {
      println("UNEXPECTED RESULT")
      false
    } else {
      true
    }
  }

  def execute(bm: SatBenchmark, timeoutInSecs: Int = DEFAULT_TIMEOUT_IN_SECS, verbose: Boolean = false): (SatBenchmark.Status, Long) = {
    val sid = bm.toIntegratedSid
    //println(s"Num FVs in origingal & integrated SID: ${bm.preds.numFV} / ${sid.numFV}")
    val timeout = Duration(timeoutInSecs, SECONDS)
    val res: DecisionProcedures.AnalysisResult = DecisionProcedures.decideInstance(
      sid,
      RunSat().getAutomaton(sid.numFV),
      timeout,
      verbose = verbose,
      reportProgress = verbose)
    val resStatus = if (res.timedOut) {
      SatBenchmark.Unknown
    } else {
      if (res.isEmpty) SatBenchmark.Unsat else SatBenchmark.Sat
    }
    (resStatus, res.analysisTime)
  }

  def parseBenchmark(file: String): SatBenchmark = {
    slcomp.parseFileToSatBenchmark(file).get
  }

  def parseAllBenchmarks(): Unit = {
    var parsed = 0
    var failed = 0
    var failedNames: List[String] = Nil
    for (bench <- allSatBenchs().sortBy(_.toString)) {
      println(s"Try parsing $bench...")
      try {
        val bm = parseBenchmark(bench.toString)
        println(s"Constructed the following benchmark:\n$bm")
        parsed += 1
      } catch {
        case _:Throwable =>
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

  def allSatBenchs(): List[File] = {
    for {
      dir <- DIRS
      file <- getListOfFiles(dir)
      if file.getName.endsWith("smt2") && file.getName != "logic.smt2"
      if !SKIP_WORSTCASE_INSTANCES || !file.getName.startsWith("succ-")
    } yield file
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

}
