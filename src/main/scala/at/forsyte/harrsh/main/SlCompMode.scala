package at.forsyte.harrsh.main

import java.io.File

import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.parsers.QueryParser.FileExtensions

import scala.concurrent.duration.{Duration, SECONDS}
import at.forsyte.harrsh.parsers.slcomp
import at.forsyte.harrsh.refinement.{DecisionProcedures, RunSat}
import at.forsyte.harrsh.util.{Combinators, IOUtils, StringUtils}

import scala.collection.mutable
import scala.util.Try

object SlCompMode {

  object params {
    // General
    val Timeout = "timeout"
    val Verbose = "verbose"
    val Debug= "debug"
    // Sat Checking
    val SatCheckingIncrementalFromNumCalls = "sat-incremental"
    // Entailment Checking
    val ComputePerSideSids = "per-side-sid"
    // Batch Mode
    val BatchBaseDir = "dir"
    val BatchTimeout = "batch-timeout"
    val BatchCategories = "categories"
    val BatchSkipWorstCase = "skip-succ"
  }

  object config {

    private val map: mutable.Map[String, Any] = mutable.Map(
      // General
      params.Timeout -> 2400,
      params.Verbose -> false,
      params.Debug -> true,
      // Sat Checking
      params.SatCheckingIncrementalFromNumCalls -> 6,
      // Entailment Checking
      params.ComputePerSideSids -> true,
      // Batch Mode
      params.BatchBaseDir -> "bench",
      params.BatchTimeout -> 5,
      params.BatchCategories -> "all",
      params.BatchSkipWorstCase -> true
    )

    override def toString: String = {
      map.toSeq.sortBy(_._1).map(p => s"  ${p._1} = ${p._2}").mkString("Config(\n", "\n", "\n)")
    }

    def set(key: String, value: Any): Unit = map.update(key, value)

    def getBoolean(key: String): Boolean = map(key).asInstanceOf[Boolean]
    def getInt(key: String): Int = map(key).asInstanceOf[Int]
    def getDuration(key: String): Duration = Duration(map(key).asInstanceOf[Int], SECONDS)
    def getString(key: String): String = map(key).asInstanceOf[String]

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
  }

  private def parseArg(arg: String): Unit = {
    try {
      val (key, eqSignAndvalue) = arg.span(_ != '=')
      val value = eqSignAndvalue.tail
      if (value == "true" || value == "false") {
        config.set(key, value == "true")
      } else {
        val asInt = Try {
          Integer.parseInt(value)
        }.toOption
        asInt match {
          case None => config.set(key, value)
          case Some(i) => config.set(key, i)
        }
      }
    } catch {
      case e: Throwable => println(s"Couldn't parse $arg as key-value pair: ${e.getMessage}")
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      return
    }

    args.drop(2) foreach parseArg

    if (config.getBoolean(params.Verbose) || config.getBoolean(params.Debug)) {
      println(config)
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
        run(args(1))
      case "check" =>
        if (args(1) == "all")
          checkAll()
        else
          check(args(1), isBatchMode = false)
      case "batch" =>
        val bmFile = args(1)
        val files = for {
          line <- IOUtils.readFile(bmFile).lines
          trimmed = line.trim
          if trimmed.nonEmpty
          if trimmed(0) != '#'
        } yield trimmed
        checkList(files.toList)
    }
  }

  private def run(file: String) = {
    val res = for {
      bm <- parseBenchmark(file)
      status <- execute(bm, isBatchMode = false)._1.toBoolean
    } yield (bm.isInstanceOf[EntailmentQuery], status)
    val output = res match {
      case None => "unknown"
      case Some((isEntailment, true)) => if (isEntailment) "unsat" else "sat"
      case Some((isEntailment, false)) => if (isEntailment) "sat" else "unsat"
    }
    println(output)
  }

  private case class BenchmarkResult(status: ProblemStatus, asExpected: Boolean, time: Long)

  private def checkAll(): Unit = {
    checkList(allSlcompBenchs().map(_.toString).sorted)
  }

  private def checkList(bms: Seq[String]): Unit = {
    val stats = for {
      bench <- bms
      res = check(bench.toString, isBatchMode = true)
      _ = println(s"${bench.toString}: Computed result: ${res.status}, as expected: ${res.asExpected}, used ${res.time}ms")
    } yield Seq(
      bench.toString,
      res.status.toString,
      if (res.status == ProblemStatus.Unknown) "n/a" else res.asExpected.toString,
      res.time.toString)

    println(s"FINISHED BENCHMARK SUITE (timeout: ${config.getDuration(params.BatchTimeout).toMillis} ms)")
    val headings = Seq("Benchmark", "Status", "As Expected", "Time")
    println(StringUtils.toTable(StringUtils.defaultTableConfigForHeadings(headings), stats))
  }

  private def check(file: String, isBatchMode: Boolean): BenchmarkResult = {
    println(s"Will check $file...")
    parseBenchmark(file) match {
      case None =>
        println(s"Couldn't parse $file")
        BenchmarkResult(ProblemStatus.Unknown, asExpected = true, 0)
      case Some(bm) =>
        checkBenchmark(bm, isBatchMode)
    }
  }

  private def checkBenchmark(bm: Query, isBatchMode: Boolean): BenchmarkResult = {
    val verbose = config.getBoolean(params.Verbose)
    if (verbose) {
      println(s"Benchmark: $bm")
      println(s"Input for refinement: $bm")
      println(s"Expected result: ${bm.status}")
    }
    val (res, time) = execute(bm, isBatchMode)
    if (verbose) println(s"Done. Result: $res")
    val deviation = if (bm.status != ProblemStatus.Unknown && bm.status != res) {
      println(if (res != ProblemStatus.Unknown) "UNEXPECTED RESULT" else "UNKNOWN")
      false
    } else {
      true
    }
    BenchmarkResult(res, deviation, time)
  }

  private def execute(bm: Query, isBatchMode: Boolean): (ProblemStatus, Long) = {
    val timeout = config.getDuration(if (isBatchMode) params.BatchTimeout else params.Timeout)
    bm match {
      case q: SatQuery => executeSatQuery(q, timeout)
      case q: EntailmentQuery => executeEntailmentQuery(q, timeout)
      case _ =>
        println("Input parsed to wrong query type.")
        (ProblemStatus.Unknown, 0)
    }
  }

  private def executeSatQuery(bm: SatQuery, timeout: Duration): (ProblemStatus, Long) = {
    val verbose = config.getBoolean(params.Verbose)
    val sid = bm.toIntegratedSid
    val res: DecisionProcedures.AnalysisResult = DecisionProcedures.decideInstance(
      sid,
      RunSat.getAutomaton,
      timeout,
      None,
      incrementalFromNumCalls = Some(config.getInt(params.SatCheckingIncrementalFromNumCalls)),
      skipSinksAsSources = true,
      verbose,
      reportProgress = verbose)
    val resStatus = if (res.timedOut) {
      ProblemStatus.Unknown
    } else {
      if (res.isEmpty) ProblemStatus.Incorrect else ProblemStatus.Correct
    }
    (resStatus, res.analysisTime)
  }

  private def executeEntailmentQuery(bm: EntailmentQuery, timeout: Duration): (ProblemStatus, Long) = {
    Combinators.tillTimeout(timeout) {
      () => statusOfQuery(bm)
    } match {
      case None =>
        println(s"Reached timeout ($timeout) on $bm")
        (ProblemStatus.Unknown, timeout.toMillis)
      case Some(res) => res
    }
  }

  private def statusOfQuery(bm: EntailmentQuery) : ProblemStatus = {
    val maybeEI = try {
      bm.toEntailmentInstance(computeSeparateSidsForEachSide = config.getBoolean(params.ComputePerSideSids))
    } catch {
      case e: Throwable =>
        println("Error: Conversion to entailment instance failed with exception" + e.getMessage)
        None
    }

    maybeEI match {
      case None =>
        println(s"Couldn't convert $bm to entailment instance")
        ProblemStatus.Unknown
      case Some(ei) =>
        val verbose = config.getBoolean(params.Verbose)
        try {
          val (res, stats) = EntailmentChecker.solve(ei, printResult = false, reportProgress = verbose, exportToLatex = false)
          if (verbose) stats.foreach(println)
          if (res) ProblemStatus.Correct else ProblemStatus.Incorrect
        } catch {
          case e: Throwable =>
            println("Error: The entailment checker crashed with exception " + e.getMessage)
            ProblemStatus.Unknown
        }
    }
  }

  private def parseBenchmark(file: String): Option[Query] = {
    try {
      slcomp.parseFileToQuery(file)
    } catch {
      case e: Throwable =>
        println(s"Parse exception: " + e.getMessage)
        None
    }
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

  private def allSlcompBenchs(): Seq[File] = {
    val dirs = config.batchDirs
    val skipWorstCaseInstances = config.getBoolean(params.BatchSkipWorstCase)
    println("Will run all benchmarks in: " + dirs.mkString(", "))
    for {
      dir <- dirs
      file <- IOUtils.getListOfFiles(dir)
      if file.getName.endsWith(FileExtensions.SlComp) && file.getName != "logic.smt2"
      if !skipWorstCaseInstances || !file.getName.startsWith("succ-")
    } yield file
  }

}
