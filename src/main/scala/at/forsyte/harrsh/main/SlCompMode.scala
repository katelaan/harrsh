package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.{EntailmentChecker, EntailmentInstance}
import at.forsyte.harrsh.parsers.QueryParser.FileExtensions

import scala.concurrent.duration.{Duration, SECONDS}
import at.forsyte.harrsh.parsers.slcomp
import at.forsyte.harrsh.refinement.{DecisionProcedures, RunSat}
import at.forsyte.harrsh.util.{Combinators, IOUtils, StringUtils}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object SlCompMode {

  val BatchExtension: String = "bms"

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
    val IsBatchMode = "is-batch-mode"
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
      params.IsBatchMode -> false,
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

    def getTimeoutForCurrentMode: Duration = {
      config.getDuration(if (getBoolean(params.IsBatchMode)) params.BatchTimeout else params.Timeout)
    }

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

    val mode = args(0) match {
      case "list" => PrintMode
      case "parse" => ParseMode
      case "run" => RunMode
      case "check" => CheckMode
      case "preproc" => PreprocMode
    }

    mode.run(args(1))
  }

  trait Mode[A]
  {
    def runOnFile(file: String): A
    def batchPostproc(resultStream: Stream[(String,A)]): Unit = {
      // Force evaluation of stream
      resultStream.last
    }
    def beforeFile(file: String): Unit = ()
    def afterFile(file: String, res: A) = ()

    def run(arg: String): Unit = {
      if (arg == "all") runAll()
      else if (arg.endsWith(BatchExtension)) runBatch(arg)
      else {
        val res = runOnFile(arg)
        afterFile(arg, res)
      }
    }

    def runList(bms: Seq[String]): Unit = {
      config.set(params.IsBatchMode, true)
      val resStream = for {
        bm <- bms.toStream
        _ = beforeFile(bm)
        res = runOnFile(bm)
        _ = afterFile(bm,res)
      } yield (bm, res)
      batchPostproc(resStream)
    }

    def runAll(): Unit = runList(allSlcompBenchsInSelectedBatchDirs())

    def runBatch(bmFile: String): Unit = {
      val files = for {
        line <- IOUtils.readFile(bmFile).lines
        trimmed = line.trim
        if trimmed.nonEmpty
        if trimmed(0) != '#'
      } yield trimmed
      runList(files.toList)
    }

    def allSlcompBenchsInSelectedBatchDirs(): Seq[String] = {
      val dirs = config.batchDirs
      val skipWorstCaseInstances = config.getBoolean(params.BatchSkipWorstCase)
      println("Will process all benchmarks in: " + dirs.mkString(", "))
      (for {
        dir <- dirs
        file <- IOUtils.getListOfFiles(dir)
        if file.getName.endsWith(FileExtensions.SlComp) && file.getName != "logic.smt2"
        if !skipWorstCaseInstances || !file.getName.startsWith("succ-")
      } yield file).map(_.toString).sorted
    }

  }

  private object PrintMode extends Mode[Unit] {
    override def runOnFile(file: String): Unit = println(file)
  }

  /**
   * Preprocessing only
   */

  private type PreprocRes = (Option[String], Option[Query])

  private object PreprocMode extends Mode[PreprocRes] {

    override def runOnFile(file: String): PreprocRes = {
      println(s"Will preprocess $file...")
      parseBenchmark(file) match {
        case None =>
          val msg = s"Couldn't parse $file"
          println(msg)
          (Some(msg), None)
        case Some(bm) =>
          (preprocQuery(bm), Some(bm))
      }
    }

    private def preprocQuery(query: Query): Option[String] = {
      query match {
        case q: SatQuery =>
          println(q.toIntegratedSid)
          None
        case q: EntailmentQuery =>
          queryToEI(q) match {
            case Failure(e) =>
              println("Exception during preprocessing: " + e.getMessage)
              Some(e.getMessage)
            case Success(ei) =>
              println(ei.prettyPrint)
              None
          }
        case _ =>
          Some("Input parsed to wrong query type.")
      }
    }

    override def batchPostproc(resultStream: Stream[(String, PreprocRes)]): Unit = {
      val (errors, successes) = resultStream.partition(pair => pair._2._1.nonEmpty)
      val numErrs = errors.size
      val numBms = errors.size + successes.size
      println(s"A total of $numErrs/${numErrs+numBms} benchmarks could not be processed:")
      errors.zipWithIndex.foreach{
        case (res, index) =>
          val (file, (Some(errorMsg), maybeQuery)) = res
          val query = maybeQuery.map(_.toString).getOrElse("parse error")
          val msg = s"${index+1} - $file:\n$query\nException: $errorMsg"
          println(msg)
      }
    }
  }

  /*
   * Check benchmarks
   */

  private case class BenchmarkResult(status: ProblemStatus, asExpected: Boolean, time: Long)

  private object CheckMode extends Mode[BenchmarkResult] {

    override def runOnFile(file: String): BenchmarkResult = {
      parseBenchmark(file) match {
        case None =>
          println(s"Couldn't parse $file")
          BenchmarkResult(ProblemStatus.Unknown, asExpected = true, 0)
        case Some(bm) =>
          checkQuery(bm)
      }
    }

    private def checkQuery(bm: Query): BenchmarkResult = {
      val verbose = config.getBoolean(params.Verbose)
      if (verbose) {
        println(s"Benchmark: $bm")
        println(s"Input for refinement: $bm")
        println(s"Expected result: ${bm.status}")
      }
      val (res, time) = execute(bm)
      if (verbose) println(s"Done. Result: $res")
      val deviation = if (bm.status != ProblemStatus.Unknown && bm.status != res) {
        println(if (res != ProblemStatus.Unknown) "UNEXPECTED RESULT" else "UNKNOWN")
        false
      } else {
        true
      }
      BenchmarkResult(res, deviation, time)
    }

    override def batchPostproc(results: Stream[(String, BenchmarkResult)]): Unit = {
      val stats = for {
        (bench, res) <- results
      } yield Seq(
        bench,
        res.status.toString,
        if (res.status == ProblemStatus.Unknown) "n/a" else res.asExpected.toString,
        res.time.toString)

      println(s"FINISHED BENCHMARK SUITE (timeout: ${config.getDuration(params.BatchTimeout).toMillis} ms)")
      val headings = Seq("Benchmark", "Status", "As Expected", "Time")
      println(StringUtils.toTable(StringUtils.defaultTableConfigForHeadings(headings), stats))
    }

    override def beforeFile(file: String): Unit = println(s"Will check $file...")

    override def afterFile(file: String, res: BenchmarkResult): Unit = {
      println(s"${file}: Computed result: ${res.status}, as expected: ${res.asExpected}, used ${res.time}ms")
    }

  }

  /*
   * Run query
   */

  private object RunMode extends Mode[Unit] {
    override def runOnFile(file: String): Unit = {
      val res = for {
        bm <- parseBenchmark(file)
        status <- execute(bm)._1.toBoolean
      } yield (bm.isInstanceOf[EntailmentQuery], status)
      val output = res match {
        case None => "unknown"
        case Some((isEntailment, true)) => if (isEntailment) "unsat" else "sat"
        case Some((isEntailment, false)) => if (isEntailment) "sat" else "unsat"
      }
      println(output)
    }

    override def batchPostproc(resultStream: Stream[(String, Unit)]): Unit = {
      println(s"FINISHED BENCHMARK SUITE (timeout: ${config.getDuration(params.BatchTimeout).toMillis} ms)")
    }

    override def beforeFile(file: String): Unit = println(s"Will run $file...")
  }

  private def execute(bm: Query): (ProblemStatus, Long) = {
    val timeout = config.getTimeoutForCurrentMode
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

  private def queryToEI(bm: EntailmentQuery) : Try[EntailmentInstance] = {
    bm.toEntailmentInstance(computeSeparateSidsForEachSide = config.getBoolean(params.ComputePerSideSids))
  }

  private def statusOfQuery(bm: EntailmentQuery) : ProblemStatus = {
    queryToEI(bm) match {
      case Failure(e) =>
        println(s"Couldn't convert $bm to entailment instance")
        ProblemStatus.Unknown
      case Success(ei) =>
        runEntailmentChecker(ei)
    }
  }

  private def runEntailmentChecker(ei: EntailmentInstance) : ProblemStatus = {
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

  /*
   * Parsing
   */

  private object ParseMode extends Mode[Option[Query]] {

    override def runOnFile(file: String): Option[Query] = parseBenchmark(file)

    override def beforeFile(file: String): Unit = println(s"Try parsing $file...")

    override def afterFile(file: String, res: Option[Query]): Unit = {
      println(s"Constructed the following benchmark:\n" + res.getOrElse("(failed)"))
    }

    override def batchPostproc(resultStream: Stream[(String, Option[Query])]): Unit = {
      var parsed = 0
      var failed = 0
      var failedNames: List[String] = Nil
      for ((bench,maybeQuery) <- resultStream) {
        maybeQuery match {
          case Some(query) =>
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

}
