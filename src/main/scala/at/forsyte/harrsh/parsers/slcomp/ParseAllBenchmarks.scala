package at.forsyte.harrsh.parsers.slcomp

import java.io.File

object ParseAllBenchmarks {

  val Dirs = List("bench/qf_shls_sat", "bench/qf_shid_sat")

  def main(args: Array[String]): Unit = {
    var parsed = 0
    var failed = 0
    var failedNames: List[String] = Nil
    for (bench <- allSatBenchs().sortBy(_.toString)) {
      println(s"Try parsing $bench...")
      try {
        val sid = parseFileToSid(bench.toString)
        println(s"Constructed the following SID:\n$sid")
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
      dir <- Dirs
      file <- getListOfFiles(dir)
      if file.getName.endsWith("smt2") && file.getName != "logic.smt2"
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
