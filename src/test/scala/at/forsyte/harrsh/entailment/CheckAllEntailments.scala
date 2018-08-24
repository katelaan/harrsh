package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.parsers.EntailmentParsers
import at.forsyte.harrsh.util.IOUtils

object CheckAllEntailments {

  def main(args: Array[String]): Unit = {

    val files = IOUtils.allFilesRecursively("examples/entailment")
    val failures = for {
      file <- files
      fileContent = IOUtils.readFile(file.toString)
      // Explicit call to get instead of <- to avoid swallowing parse errors
      instance = EntailmentParsers.parse(fileContent).get
      if !EntailmentChecker.check(file.toString, instance, reportProgress = false)
    } yield file.toString

    if (failures.isEmpty) {
      println("All entailment checks returned the expected results.")
    } else {
      println("Entailment checks on the following files retuend unexpected results:")
      println(failures.map(name => s" - $name").mkString("\n"))
    }

  }

}
