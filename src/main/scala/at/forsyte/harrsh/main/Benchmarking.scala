package at.forsyte.harrsh.main

import java.io.File

import at.forsyte.harrsh.heapautomata._
import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.seplog.parsers.{CyclistSIDParser, DefaultSIDParser}
import at.forsyte.harrsh.util.IOUtils._
import at.forsyte.harrsh.main.FV._

import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by jkatelaa on 10/20/16.
  */
object Benchmarking extends SlexLogging {

  val PathToDatastructureExamples = "examples" + File.separator + "datastructures"
  val PathToCyclistExamples = "examples" + File.separator + "cyclist"

  val ResultFile = "benchmark-results.tex"

  val CyclistSuffix = "defs"
  val SidSuffix = "sid"

  val DefaultTimeout = Duration(120, scala.concurrent.duration.SECONDS)

  type Result = (Boolean,Long)

  //def main(args : Array[String]) = generateAndPrintTasks()

  def runBenchmarkFile(file : String, timeout : Duration = DefaultTimeout, verbose : Boolean = false, reportProgress : Boolean = false) = {
    println("Will run all benchmarks in " + file)
    val tasks = readTasksFromFile(file)
    runBenchmarks(tasks, timeout, verbose, reportProgress)
  }

  def generateAndPrintTasks() = {
    println(generateTasks() map (_.toString) mkString ("\n"))
  }

  private def readTasksFromFile(filename : String) : Seq[TaskConfig] = {
    val content = readFile(filename)
    val lines = content.split('\n').map(_.trim).filterNot(_.isEmpty)
    val otasks = lines map TaskConfig.fromString

    if (otasks.exists(_.isEmpty)) {
      //println(otasks.mkString("\n"))
      throw new Exception("Error while parsing benchmarks")
    } else {
      otasks map (_.get)
    }
  }

  private val headings = Seq("File", "Property", "Result", "Time in ms")

  private def printBenchmarkResults(results: List[(TaskConfig, Result)]): Unit = {

    def inColumns(cols : Seq[(String,Int)]) : String = if (cols.isEmpty) "|" else "|" + (" "*(Math.max(0,cols.head._2 - cols.head._1.length))) + cols.head._1 + inColumns(cols.tail)

    val cols = Seq(30,20,20,10)

    val delimLine = "+" + "-"*(cols.sum+cols.size-1) + "+"

    println(delimLine)
    println(inColumns(headings zip cols))
    println(delimLine)
    for ( (task,res) <- results ) {
      val entries : Seq[String] = Seq(task.fileName.split("/").last, task.decisionProblem.toString, task.decisionProblem.resultToString(res._1), ""+res._2)
      println(inColumns(entries zip cols))
    }
    println(delimLine)

  }

  private def writeLatexFile(results: List[(TaskConfig, Result)], summary: String): Unit = {
    val preamble =
      """
        |\documentclass{article}
        |\begin{document}
        |\begin{tabular}{llll}
      """.stripMargin
    val header = headings.mkString(" & ") + "\\\\\n"
    val resultLines = (for {
      (task,res) <- results
      entries : Seq[String] = Seq(task.fileName.split("/").last, task.decisionProblem.toString, task.decisionProblem.resultToString(res._1), ""+res._2)
    } yield entries.mkString("", " & ", "\\\\")).mkString("\n")
    val ending ="\n\\end{tabular}\n\\begin{itemize}\n" + summary.split("\n").map("\\item "+_).mkString("\n") + "\n\\end{itemize}\n\\end{document}"

    writeFile(ResultFile, preamble + header + resultLines + ending)
  }

  private def runBenchmarks(tasks : Seq[TaskConfig], timeout : Duration = DefaultTimeout, verbose : Boolean, reportProgress : Boolean): Unit = {

    val globalStartTime = System.currentTimeMillis()
    var analysisTime : Long = 0

    var results : List[(TaskConfig,Result)] = Nil
    var numTimeouts : Int = 0

    for (task <- tasks) {
      val (sid, ha) = prepareBenchmark(task)
      if (verbose) {
        printLinesOf('%', 1)
        println("File: " + task.fileName)
        printLinesOf('%', 1)
        println("Will run automaton " + ha + " on " + sid)
      } else {
        print("Running " + task.decisionProblem + " on " + task.fileName + "...")
      }

      val startTime = System.currentTimeMillis()

      val f: Future[Boolean] = Future {
        new RefinementAlgorithms(sid, ha).onTheFlyEmptinessCheck(reportProgress = reportProgress)
      }

      val result = try {
        val isEmpty = Await.result(f, timeout)
        val endTime = System.currentTimeMillis()
        println("Finished in " + (endTime - startTime) + "ms")
        analysisTime += (endTime - startTime)
        (isEmpty, endTime - startTime)
      } catch {
        case e : TimeoutException =>
          println("reached timeout (" + timeout + ")")
          numTimeouts += 1
          (true, timeout.toMillis)
      }

      results = (task, result) :: results

    }

    val globalEndTime = System.currentTimeMillis()
    println()
    printLinesOf('#', 2)
    println("FINISHED BENCHMARK SUITE")
    printLinesOf('#', 2)
    println()

    printBenchmarkResults(results.reverse)

    println()
    val totalTime = globalEndTime-globalStartTime
    val summary = ("Completed number of benchmarks: " + (tasks.size - numTimeouts) + " / " + tasks.size + "\n" +
        "Timeout (TO):             " + timeout.toMillis + " ms\n"
      + "Total time:               " + totalTime + " ms\n"
      + "Analysis time (with TOs): " + (analysisTime+timeout.toMillis*numTimeouts) + " ms\n"
      + "Analysis time (w/o TOs):  " + analysisTime + " ms")
    println(summary)
    println()
    println("Will write results to " + ResultFile)
    writeLatexFile(results.reverse, summary)

  }

  def prepareBenchmark(task : TaskConfig) : (SID, HeapAutomaton) = {

    val parser = if (task.fileName.endsWith(CyclistSuffix)) {
      logger.debug("File ends in .defs, will assume cyclist format")
      CyclistSIDParser.run _
    } else {
      logger.debug("Assuming standard SID format")
      DefaultSIDParser.run _
    }

    val content = readFile(task.fileName)

    parser(content) match {
      case Some((sid,numFV)) =>
        (sid, task.decisionProblem.getAutomaton(numFV))
      case None =>
        println("Parsing failed, exiting")
        throw new Exception("Parsing failed during benchmark run for task " + task)
    }

  }

  private def generateTasks() =
    for {
      automaton <- Seq(RunHasPointer(), RunTracking(Set(fv(1)), Set()), RunSat(), RunUnsat(), RunEstablishment(), RunNonEstablishment(), RunReachability(fv(1), fv(0)), RunGarbageFreedom(), RunAcyclicity())
      file <- getListOfFiles(PathToDatastructureExamples).sortBy(_.getName) ++ getListOfFiles(PathToCyclistExamples).sortBy(_.getName)
    } yield TaskConfig(file.getAbsolutePath, automaton, None)

}
