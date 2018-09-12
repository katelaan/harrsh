package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentInstance
import at.forsyte.harrsh.parsers.EntailmentParsers
import at.forsyte.harrsh.util.IOUtils

import scala.util.{Failure, Success, Try}

object EntailmentBatchMode {

  val PathToDefaultEntailmentBenchmarks = "examples/entailment"

  def main(args: Array[String]): Unit = {
    runAllEntailmentsInPath(PathToDefaultEntailmentBenchmarks)
  }

  def runAllEntailmentsInPath(path: String): Unit = {
    val files = IOUtils.allFilesRecursively(path)
    val failures = (for {
      file <- files
      if !file.toString.contains("todo")
      fileContent = IOUtils.readFile(file.toString)
      instance = Try { println(s"Checking $file..."); EntailmentParsers.parse(fileContent) }
      failure <- collectFailure(file.toString, instance)
    } yield failure).toList

    if (failures.isEmpty) {
      println("All entailment checks returned the expected results.")
    } else {
      println("Entailment checks on the following files went wrong:")
      println(failures.map{
        case (name, msg) => s" - $name: $msg"
      }.mkString("\n"))
    }
  }

  private def collectFailure(file: String, tryInstance: Try[Option[EntailmentInstance]]) : Option[(String,String)] = {
    val errorMsg = tryInstance match {
      case Failure(exception) => Some(s"Exception during parsing: ${exception.getMessage}")
      case Success(maybeInstance) =>
        maybeInstance match {
          case Some(instance) =>
            Try {
              !EntailmentChecker.check(file.toString, instance, reportProgress = false)
            } match {
              case Failure(exception) => Some(s"Exception during entailment check: ${exception.getMessage}")
              case Success(unexpectedResult) => if (unexpectedResult) Some("Unexpected result") else None
            }
          case None =>
            Some("Parse error")
        }
    }
    errorMsg map { msg => (file, msg) }
  }


}
