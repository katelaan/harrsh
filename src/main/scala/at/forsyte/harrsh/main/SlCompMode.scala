package at.forsyte.harrsh.main

import java.io.File

import at.forsyte.harrsh.parsers.slcomp

object SlCompMode {

  val Dirs = List("bench/qf_shls_sat", "bench/qf_shid_sat")

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      return
    }

    args(0) match {
      case "parse" =>
        if (args(1) == "all")
          parseAllBenchmarks()
        else
          parseBenchmark(args(1))
      case "run" =>
        run(args(1))
      case "check" =>
        if (args(1) == "all")
          checkAll()
        else
          check(args(1))
    }
  }

  def run(file: String) = {

  }

  def checkAll(): Unit = {
    ???
  }

  def check(file: String): Unit = {

  }

  def parseBenchmark(file: String): Unit = {
    val bm = slcomp.parseFileToSatBenchmark(file)
    println(s"Constructed the following benchmark:\n$bm")
  }

  def parseAllBenchmarks(): Unit = {
    var parsed = 0
    var failed = 0
    var failedNames: List[String] = Nil
    for (bench <- allSatBenchs().sortBy(_.toString)) {
      println(s"Try parsing $bench...")
      try {
        val bm = slcomp.parseFileToSatBenchmark(bench.toString)
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
