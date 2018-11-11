package at.forsyte.harrsh.main

import java.io.FileNotFoundException

import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.parsers.{ModelParser, SIDParsers}
import at.forsyte.harrsh.util.IOUtils._
import at.forsyte.harrsh.util.StringUtils._
import at.forsyte.harrsh.refinement.DecisionProcedures.{AnalysisResult, AnalysisStatistics}
import at.forsyte.harrsh.modelchecking.Model
import at.forsyte.harrsh.heapautomata.HeapAutomaton
import at.forsyte.harrsh.refinement.AutomatonTask

/**
  * Created by jens on 2/24/17.
  */
object MainIO extends HarrshLogging {

  val ResultFile = "benchmark-results.tex"

  object FileExtensions {
    val Cyclist = "defs"
    val HarrshSid = "sid"
    val HarrshEntailment = "hrs"
    val SlComp = "smt2"
  }

  private val Headings = Seq("File", "Property", "Result", "Time in ms")

  /**
   * Parse file into SID
   */
  def getSidFromFile(fileName : String) : SID = {
    val parser = if (fileName.endsWith(FileExtensions.Cyclist)) {
      logger.debug("File ends in .defs, will assume cyclist format")
      SIDParsers.CyclistSIDParser
    } else {
      logger.debug("Assuming standard SID format")
      SIDParsers.DefaultSIDParser
    }

    val content = readFile(fileName)

    parser.runOnSID(content) match {
      case Some(sid) =>
        sid
      case None =>
        printWarningToConsole("Parsing the SID failed, exiting")
        throw new Exception("Parsing of file '" + fileName + "'failed")
    }
  }

  def getSidAndAutomaton(sidFile : String, prop: AutomatonTask) : (SID, HeapAutomaton) = {
    val sid = MainIO.getSidFromFile(sidFile)
    (sid, prop.getAutomaton)
  }

  def getModelFromFile(fileName : String) : Model = {
    val content = readFile(fileName)

    ModelParser.run(content) match {
      case Some(model) =>
        model
      case None =>
        printWarningToConsole("Parsing the model failed, exiting")
        throw new Exception("Parsing of file '" + fileName + "'failed")
    }
  }

  def readTasksFromFile(filename : String) : Seq[TaskConfig] = {
    val content = try {
      readFile(filename)
    } catch {
      case e : FileNotFoundException =>
        printWarningToConsole("File '" + filename + "' does not exist.")
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
    val minColLengths = Seq(20, 10, 20, 10)
    val alignment = Seq(AlignLeft, AlignRight, AlignRight, AlignRight)
    val entries = for ((task,res) <- results) yield Seq(task.fileName.split("/").last, task.decisionProblem.toString, task.decisionProblem.resultToString(res.isEmpty), "" + res.analysisTime)
    val config = TableConfig(Headings, minColLengths, alignment)
    println(toTable(config, entries))

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

  def writeBenchmarkFile(results: Seq[(TaskConfig,AnalysisResult)], fileName : String): Unit = {
    val content = results.map{
      case (taskConfig, result) => taskConfig.fileName + "; " + taskConfig.decisionProblem + "; " + (if (result.timedOut) "???" else !result.isEmpty)
    }.mkString("\n")

    writeFile(fileName, content)
  }

}
