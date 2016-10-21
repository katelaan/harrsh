package at.forsyte.harrsh.main

import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by jens on 10/19/16.
  */
object Harrsh {

  def main(args : Array[String]) = {
    if (args.length >= 2 && ((args(0) == "--tasks") || (args(0) == "-t"))) {
      try {
        val timeout = if (args.length >= 4 && args(2) == "--timeout") {
          tryParseAsInt(args(3)) match {
            case Some(i) =>  Duration(i, SECONDS)
            case None =>
              println("Could not parse '" + args(3) + "' as timeout. Please pass a positive integer (denoting the timeout in seconds)")
              Benchmarking.DefaultTimeout
          }
        } else {
          Benchmarking.DefaultTimeout
        }
        println("Working with timeout: " + timeout)

        Benchmarking.runBenchmarkFile(args(1), timeout, args.length >= 5 && args(4) == "--verbose")
      } catch {
        case e : Throwable =>
          println("An exception occurred during benchmarking: ")
          println(e)
      }
    }
    else {
      printUsage()
    }
  }

  private def tryParseAsInt(s : String) : Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch {
      case _ : Throwable => None
    }
  }

  private def printUsage() = println("Usage: --tasks <relative-path-to-file-with-list-of-tasks> [--timeout <timeout-in-seconds> [--verbose]]")

}
