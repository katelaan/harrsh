package at.forsyte.harrsh.main

/**
  * Created by jens on 10/19/16.
  */
object Harrsh {

  def main(args : Array[String]) = {
    if (args.length == 2 && ((args(0) == "--tasks") || (args(0) == "-t"))) {
      try {
        Benchmarking.runBenchmarkFile(args(1))
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

  private def printUsage() = println("Usage: --tasks <relative-path-to-file-with-list-of-tasks>")

}
