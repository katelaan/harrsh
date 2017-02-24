package at.forsyte.harrsh.main

import at.forsyte.harrsh.heapautomata.{AutomatonTask, RefinementAlgorithms, RunSat}
import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.util.IOUtils

import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by jens on 10/19/16.
  */
object Harrsh {

  val PreviousSidFileName = "LAST"
  val DefaultAnalysisTimeout = Duration(5, scala.concurrent.duration.SECONDS)

  def main(args : Array[String]) : Unit = {
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

    var file : String = ""

    def tryParseFileArg(long : String, short : String) : Boolean = {
      if (file.isEmpty) {
        file = parseSwitchWithArg(long, short, "", setSuccess = true)
        !file.isEmpty
      } else {
        false
      }
    }

    val help : Boolean = parseSwitch("--help", "-h")

    val runBatch = tryParseFileArg("--batch", "-b")
    val runRefinement = tryParseFileArg("--refine", "-r")
    val decideProp = tryParseFileArg("--decide", "-d")
    val showSID = tryParseFileArg("--show", "-s")
    val unfoldSID = tryParseFileArg("--unfold", "-u")
    val analyzeSID = tryParseFileArg("--analyze", "-a")

    val propertyString = parseSwitchWithArg("--prop", "-p", "SAT")
    val prop = AutomatonTask.fromString(propertyString).getOrElse({
      IOUtils.printWarningToConsole("No valid property specified via --prop, will default to SAT")
      RunSat()
    })

    val applicableDefaultTimeout = if (analyzeSID) DefaultAnalysisTimeout else DecisionProcedures.DefaultTimeout

    val timeoutString : String = parseSwitchWithArg("--timeout", "-t", "" + applicableDefaultTimeout.toSeconds)
    val timeout = tryParseAsInt(timeoutString) match {
                    case Some(i) =>  Duration(i, SECONDS)
                    case None =>
                      println("Could not parse argument to --timeout; will use default " + applicableDefaultTimeout + ". Please pass a positive integer (denoting the timeout in seconds)")
                      applicableDefaultTimeout
                  }

    val DefaultDepth = 3
    val unfoldString : String = parseSwitchWithArg("--depth", "-d", ""+DefaultDepth)
    val unfoldLimit = tryParseAsInt(unfoldString) match {
      case Some(i) =>  i
      case None =>
        println("Could not parse argument to --limit; will use default " + DefaultDepth + ". Please pass a positive integer (denoting the maximum unfolding depth)")
        DefaultDepth
    }
    val returnReducedOnly = parseSwitch("--reduced", "-red")

    val verbose = parseSwitch("--verbose", "-v")
    val reportProgress = parseSwitch("--showprogress", "-sp")

    if (!success) {
      printUsage()
    } else {
      try {
        if (runBatch) {
          // Batch mode
          println("Will run all benchmarks in " + file)
          val tasks = MainIO.readTasksFromFile(file)
          val (results,stats) = DecisionProcedures.decideInstances(tasks, timeout, verbose, reportProgress)
          MainIO.printAnalysisResults(results, stats)
        } else if (runRefinement) {
          // Refinement mode
          println("Will refine SID definition in file " + file + " by " + prop)
          val sid = RefinementAlgorithms.refineSID(file, prop, timeout, reportProgress = reportProgress)
          sid match {
            case Some(vsid) =>
              println(vsid._1)
              IOUtils.writeFile(PreviousSidFileName, SID.toHarrshFormat(vsid._1))

              if (vsid._2) {
                IOUtils.printWarningToConsole("Language of refined SID is empty (no rules for start predicate '" + vsid._1.startPred + "').")
              }
            case None =>
              IOUtils.printWarningToConsole("Refinement failed.")
          }
        } else if (decideProp) {
          // Decision procedure mode
          val task = TaskConfig(file, prop, None)
          val result = DecisionProcedures.decideInstance(task, timeout, verbose, reportProgress)
          MainIO.printAnalysisResult(task, result)
        } else if (showSID) {
          // Print mode
          val (sid,_) = MainIO.getSidFromFile(file)
          println(sid)
          IOUtils.writeFile(PreviousSidFileName, SID.toHarrshFormat(sid))
        } else if (unfoldSID) {
          // Unfold mode
          val (sid,_) = MainIO.getSidFromFile(file)
          println(SID.unfold(sid, unfoldLimit, returnReducedOnly).mkString("\n"))
        } else if (analyzeSID) {
          val (sid,numfv) = MainIO.getSidFromFile(file)
          println(RefinementAlgorithms.performFullAnalysis(sid, numfv, timeout))
        } else {
          // Unknown task
          IOUtils.printWarningToConsole("Terminating with unspecified task.")
        }
      } catch {
        case e : Throwable =>
          println("Terminating with exception: " + e.getMessage)
          // TODO Only do this in debug mode
//          println("Terminating with " + e.getClass.toString + " (Message: " + e.getMessage + ")")
//          throw e
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

  private def printUsage() = {
    println("This is HARRSH. Usage:")
    println()
    println("Refinement mode:")
    println("  --refine <relative-path-to-sid-file> --prop <property>      refine sid by prop")
    println()
    println("Decision procedure mode:")
    println("  --decide <relative-path-to-sid-file> --prop <property>   check if sid has prop")
    println()
    println("Batch / benchmarking mode:")
    println("  --batch <relative-path-to-file-with-list-of-tasks>          batch benchmarking")
    println()
    println("Analysis mode:")
    println(" --analyze <relative-path-to-sid-file>           analyze robustness of given sid")
    println()
    println("Exploration mode:")
    println("  --show <relative-path-to-sid-file>                        print sid to std out")
    println("  --unfold <relative-path-to-sid-file>     generate all unfoldings of the sid...")
    println("     [--depth <depth>]                    ...up to depth <depth> (default: 3)...")
    println("     [--reduced]                          ...showing only reduced symbolic heaps")
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
