package at.forsyte.harrsh.main

import java.io.FileNotFoundException

import at.forsyte.harrsh.parsers.ModelParser
import at.forsyte.harrsh.util.IOUtils._
import at.forsyte.harrsh.util.StringUtils._
import at.forsyte.harrsh.refinement.DecisionProcedures.{AnalysisResult, AnalysisStatistics}
import at.forsyte.harrsh.modelchecking.Model

/**
  * Created by jens on 2/24/17.
  */
object MainIO extends HarrshLogging {

  val ResultFile = "benchmark-results.tex"

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

  def readTasksFromFile(filename : String) : Seq[RefinementQuery] = {
    val content = try {
      readFile(filename)
    } catch {
      case e : FileNotFoundException =>
        printWarningToConsole(s"File '$filename' does not exist.")
        throw e
      case e : Throwable =>
        throw e
    }

    val lines = content.split('\n').map(_.trim).filterNot(_.isEmpty)
    val otasks = lines map { taskspec =>
      try {
        RefinementQuery.fromTaskSpecString(taskspec)
      } catch {
        case e: Throwable =>
          printWarningToConsole(s"Could not parse task '$taskspec': ${e.getMessage}")
          None
      }
    }

    if (otasks.exists(_.isEmpty)) {
      //println(otasks.mkString("\n"))
      printWarningToConsole("Will skip one or more tasks because of parse errors.")
    }
    otasks.toSeq.flatten
  }

  /**
    * Prints table + writes Latex file with results
    * @param results Results of executing the tasks
    * @param times Statistics about analysis times
    */
  def printAnalysisResults(results: Seq[(RefinementQuery, AnalysisResult)], times : AnalysisStatistics): Unit = {
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
    writeLatexFileForRefinementResults(results, summary)
  }

  def printAnalysisResult(task : RefinementQuery, result : AnalysisResult): Unit = {
    printResultTable(Seq((task,result)))
  }

  private val Headings = Seq("File", "Property", "Result", "Time in ms")

  private def tableEntries(results: Seq[(RefinementQuery, AnalysisResult)]) : Seq[Seq[String]] = {
    for ((query,res) <- results) yield Seq(
      query.fileNameString.split("/").last,
      query.taskString,
      query.task.map(_.resultToString(res.isEmpty)).getOrElse(""),
      "" + res.analysisTime
    )
  }

  private def printResultTable(results: Seq[(RefinementQuery, AnalysisResult)]): Unit = {
    val minColLengths = Seq(20, 10, 20, 10)
    val alignment = Seq(AlignLeft, AlignRight, AlignRight, AlignRight)
    val entries = tableEntries(results)
    val config = TableConfig(Headings, minColLengths, alignment)
    println(toTable(config, entries))

  }

  private def writeLatexFileForRefinementResults(results: Seq[(RefinementQuery, AnalysisResult)], summary: String): Unit = {
    val resultStrings = tableEntries(results)
    val bulletPoints = summary.split("\n")
    writeLatexFile(ResultFile, Headings, resultStrings, bulletPoints)
  }

  def writeLatexFile(resultFile: String, headings: Seq[String], results: Seq[Seq[String]], bulletPoints: Seq[String] = Seq.empty): Unit = {
    val preamble =
      s"""
        |\\documentclass{article}
        |\\begin{document}
        |\\begin{tabular}{${"l"*headings.size}}
      """.stripMargin
    val header = headings.mkString(" & ") + "\\\\\n"
    val resultLines = (for {
      row <- results
    } yield row.mkString("", " & ", "\\\\")).mkString("\n")
    val list = if (bulletPoints.nonEmpty) {
      "\\begin{itemize}\n" + bulletPoints.map("\\item "+_).mkString("\n") + "\n\\end{itemize}\n"
    } else ""
    val ending = s"\n\\end{tabular}\n$list\\end{document}"

    writeFile(resultFile, preamble + header + resultLines + ending)
  }

  def writeBenchmarkFile(results: Seq[(RefinementQuery,AnalysisResult)], fileName : String): Unit = {
    val content = results.map{
      case (taskConfig, result) => taskConfig.fileName + "; " + taskConfig.taskString + "; " + (if (result.timedOut) "???" else !result.isEmpty)
    }.mkString("\n")

    writeFile(fileName, content)
  }

}
