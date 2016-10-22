package at.forsyte.harrsh.main

import at.forsyte.harrsh.heapautomata.{AutomatonTask, HeapAutomaton, RefinementAlgorithms, RunSat}
import at.forsyte.harrsh.seplog.inductive.SID

import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by jens on 10/19/16.
  */
object Harrsh {

  def main(args : Array[String]) = {
    var success : Boolean = false

    def parseSwitch[A](long : String, short : String) : Boolean = args.contains(long) || args.contains(short)

    def parseSwitchWithArg(long : String, short : String, default : String, setSuccess : Boolean = false) : String = {
      val arg = Math.max(args.indexOf(long), args.indexOf(short))
      if (arg > -1 && args.length > arg + 1) {
        println("Found " + long + " at index " + arg + "; will return " + args(arg+1))
        if (setSuccess) success = true
        args(arg+1)
      } else default
    }

    var help : Boolean = parseSwitch("--help", "-h")

    var file : String = parseSwitchWithArg("--batch", "-b", "", setSuccess = true)
    val runBatch = if (!file.isEmpty) {
      true
    } else {
      file = parseSwitchWithArg("--refine", "-r", "", setSuccess = true)
      false
    }

    val propertyString = parseSwitchWithArg("--prop", "-p", "SAT")
    val prop = AutomatonTask.fromString(propertyString).getOrElse(RunSat())

    val timeoutString : String = parseSwitchWithArg("--timeout", "-t", "120")
    val timeout = tryParseAsInt(timeoutString) match {
                    case Some(i) =>  Duration(i, SECONDS)
                    case None =>
                      println("Could not parse '" + args(3) + "' as timeout. Please pass a positive integer (denoting the timeout in seconds)")
                      Benchmarking.DefaultTimeout
                  }

    val verbose = parseSwitch("--verbose", "-v")
    val reportProgress = parseSwitch("--showprogress", "-sp")

    if (!success) {
      printUsage()
    } else {
      if (runBatch) {
        Benchmarking.runBenchmarkFile(file, timeout, verbose = verbose, reportProgress = reportProgress)
      } else {

        /*val sid = RefinementAlgorithms.refineSID(file, prop, timeout, reportProgress = reportProgress)
        sid match {
          case Some(vsid) =>
            println("Refined SID: ")
            println(sid)
          case None =>
            println("Refinement failed.")
        }*/
        println("SID refinement not yet supported via the CLI")

      }
    }
  }

  private def tryParseAsInt(s : String) : Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch {
      case _ : Throwable => None
    }
  }

  private def printUsage() = println("Usage: --batch <relative-path-to-file-with-list-of-tasks> [--timeout <timeout-in-seconds> [--verbose]]")

}
