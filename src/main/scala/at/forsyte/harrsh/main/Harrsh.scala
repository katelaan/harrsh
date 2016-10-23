package at.forsyte.harrsh.main

import java.io.FileNotFoundException

import at.forsyte.harrsh.heapautomata.{AutomatonTask, HeapAutomaton, RefinementAlgorithms, RunSat}
import at.forsyte.harrsh.seplog.inductive.SID

import scala.concurrent._
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.ExecutionContext.Implicits.global

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
        //println("Found " + long + " at index " + arg + "; will return " + args(arg+1))
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

        println("Will refine SID definition in file " + file + " by " + prop)
        val sid = refineSID(file, prop, timeout, reportProgress = reportProgress)
        sid match {
          case Some(vsid) =>
            println(vsid)
          case None =>
            println("Refinement failed.")
        }

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

  private def refineSID(file : String, property : AutomatonTask, timeout : Duration, reportProgress : Boolean) : Option[SID] = {

    val task = TaskConfig(file, property, None)
    try {
      val (sid, ha) = Benchmarking.prepareBenchmark(task)

      val f: Future[SID] = Future {
        new RefinementAlgorithms(sid, ha).refineSID(reportProgress = reportProgress)
      }

      try {
        val sid = Await.result(f, timeout)
        Some(sid)
      } catch {
        case e : TimeoutException =>
          println("reached timeout (" + timeout + ")")
          None
      }

    } catch {
      case e : FileNotFoundException =>
        println("Could not open file " + file)
        None
    }
  }

  private def printUsage() = {
    println("This is HARRSH. Usage:")
    println()
    println("Batch / becnhmarking mode:")
    println("  --batch <relative-path-to-file-with-list-of-tasks>          batch benchmarking")
    println()
    println("Refinement mode:")
    println("  --refine <relative-path-to-sid-file> --prop <property>      refine sid by prop")
    println()
    println("Optional arguments:")
    println("  --timeout <timeout in s                                       optional timeout")
    println("  --showprogress                                    print progress of refinement")
    println("  --verbose                                                  produce more output")
    println()
    println("Properties:")
    println("ACYC               Weakly acyclic unfoldings")
    println("EST                Established unfoldings")
    println("GF                 Garbage-free unfoldings")
    println("HASPTR             Unfoldings that allocate memory")
    println("NON-EST            Non-established unfoldings")
    println("REACH(a,b)         Unfoldings where b is reachable from a, for a,b in {x1,x2,..,}")
    println("SAT                Satisfiable unfoldings")
    println("TRACK(a,b,...)     Unfoldings in which free variables a,b,... are def. allocated")
    println("UNSAT              Unsatisfiable unfoldings")
  }

}
