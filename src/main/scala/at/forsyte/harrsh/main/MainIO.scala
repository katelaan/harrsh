package at.forsyte.harrsh.main

import java.io.FileNotFoundException

import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.seplog.parsers.{CyclistSIDParser, DefaultSIDParser, ModelParser}
import at.forsyte.harrsh.util.IOUtils
import at.forsyte.harrsh.util.IOUtils._
import at.forsyte.harrsh.refinement.DecisionProcedures.{AnalysisResult, AnalysisStatistics}
import at.forsyte.harrsh.entailment.Model
import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.refinement.AutomatonTask

/**
  * Created by jens on 2/24/17.
  */
object MainIO extends HarrshLogging {

  val ResultFile = "benchmark-results.tex"

  val CyclistSuffix = "defs"
  val SidSuffix = "sid"

  private val Headings = Seq("File", "Property", "Result", "Time in ms")

  /*
   * Returns SID + number of free variables
   */
  def getSidFromFile(fileName : String) : SID = {
    val parser = if (fileName.endsWith(CyclistSuffix)) {
      logger.debug("File ends in .defs, will assume cyclist format")
      CyclistSIDParser.run _
    } else {
      logger.debug("Assuming standard SID format")
      DefaultSIDParser.run _
    }

    val content = readFile(fileName)

    parser(content) match {
      case Some(sid) =>
        sid
      case None =>
        IOUtils.printWarningToConsole("Parsing the SID failed, exiting")
        throw new Exception("Parsing of file '" + fileName + "'failed")
    }
  }

  def getSidAndAutomaton(sidFile : String, prop: AutomatonTask) : (SID, HeapAutomaton) = {
    val sid = MainIO.getSidFromFile(sidFile)
    (sid, prop.getAutomaton(sid.numFV))
  }

  def getModelFromFile(fileName : String) : Model = {
    val content = readFile(fileName)

    ModelParser.run(content) match {
      case Some(model) =>
        model
      case None =>
        IOUtils.printWarningToConsole("Parsing the model failed, exiting")
        throw new Exception("Parsing of file '" + fileName + "'failed")
    }
  }

  def readTasksFromFile(filename : String) : Seq[TaskConfig] = {
    val content = try {
      readFile(filename)
    } catch {
      case e : FileNotFoundException =>
        IOUtils.printWarningToConsole("File '" + filename + "' does not exist.")
        throw e
      case e : Throwable =>
        throw e
    }

    val lines = content.split('\n').map(_.trim).filterNot(_.isEmpty)
    val otasks = lines map TaskConfig.fromString

    if (otasks.exists(_.isEmpty)) {
      //println(otasks.mkString("\n"))
      throw new Exception("Error while parsing benchmarks")
    } else {
      otasks map (_.get)
    }
  }

  /**
    * Prints table + writes Latex file with results
    * @param results Results of executing the tasks
    * @param times Statistics about analysis times
    */
  def printAnalysisResults(results: Seq[(TaskConfig, AnalysisResult)], times : AnalysisStatistics): Unit = {
    // Print statistics of benchmark suite
    println()
    printLinesOf('#', 2)
    println("FINISHED BENCHMARK SUITE")
    printLinesOf('#', 2)
    println()

    printResultTable(results)

    println()
    val totalTime = times.globalEndTime - times.globalStartTime
    val summary = ("Completed number of benchmarks: " + (results.size - times.numTimeouts) + " / " + results.size + "\n" +
      "Timeout (TO):             " + times.timeout.toMillis + " ms\n"
      + "Total time:               " + totalTime + " ms\n"
      + "Analysis time (with TOs): " + (times.analysisTime + times.timeout.toMillis * times.numTimeouts) + " ms\n"
      + "Analysis time (w/o TOs):  " + times.analysisTime + " ms")
    println(summary)
    println()
    println("Will write results to " + ResultFile)
    writeLatexFile(results, summary)
  }

  def printAnalysisResult(task : TaskConfig, result : AnalysisResult): Unit = {
    printResultTable(Seq((task,result)))
  }

  private def printResultTable(results: Seq[(TaskConfig, AnalysisResult)]): Unit = {
    val cols = Seq(30,20,20,10)
    val entries = for ((task,res) <- results) yield Seq(task.fileName.split("/").last, task.decisionProblem.toString, task.decisionProblem.resultToString(res.isEmpty), "" + res.analysisTime)
    println(toTable(Headings, cols, entries))

  }

  private def writeLatexFile(results: Seq[(TaskConfig, AnalysisResult)], summary: String): Unit = {
    val preamble =
      """
        |\documentclass{article}
        |\begin{document}
        |\begin{tabular}{llll}
      """.stripMargin
    val header = Headings.mkString(" & ") + "\\\\\n"
    val resultLines = (for {
      (task,res) <- results
      entries : Seq[String] = Seq(task.fileName.split("/").last, task.decisionProblem.toString, task.decisionProblem.resultToString(res.isEmpty), ""+res.analysisTime)
    } yield entries.mkString("", " & ", "\\\\")).mkString("\n")
    val ending ="\n\\end{tabular}\n\\begin{itemize}\n" + summary.split("\n").map("\\item "+_).mkString("\n") + "\n\\end{itemize}\n\\end{document}"

    writeFile(ResultFile, preamble + header + resultLines + ending)
  }

}
