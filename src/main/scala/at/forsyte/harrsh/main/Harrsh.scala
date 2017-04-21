package at.forsyte.harrsh.main

import at.forsyte.harrsh.heapautomata.{AutomatonTask, RefinementAlgorithms}
import at.forsyte.harrsh.seplog.inductive.SID
import at.forsyte.harrsh.util.{Combinators, IOUtils}

import scala.concurrent.duration.{Duration, SECONDS}
import scalaz.State
import scalaz.State._

/**
  * The main command-line interface of Harrsh.
  */
object Harrsh {

  val PreviousSidFileName = "LAST"

  val S = scalaz.StateT.stateMonad[Config]

  def main(args : Array[String]) : Unit = {

    val config: Config = parseAll(args)
    //println(config)

    // Run in specified mode unless something is missing from the config
    if (config.mode != Help() && config.oFile.isEmpty) {
      println("No file specified => Terminating")
    }
    else if (config.mode.requiresProp && config.oProp.isEmpty) {
      println("No property specified => Terminating")
    } else {
      Combinators.swallowExceptions(run, config.debug)(config)
    }
  }

  /**
    * Parse command line arguments
    * @param args Array of arguments
    * @return Config built from arguments
    */
  def parseAll(args : Array[String]) : Config = parseAllAux(args).run(Config.DefaultConfig)._1

  private def parseAllAux(args : Array[String]) : State[Config, Unit] = {

    def parseSwitchWithArg(long: String, short: String, default: String): String = {
      val arg = Math.max(args.indexOf(long), args.indexOf(short))
      if (arg > -1 && args.length > arg + 1) args(arg + 1) else default
    }

    def parseSwitch[A](long: String, short: String, update : Config => Config): State[Config, Unit] = for {
      cnf <- get
      _ <- modify[Config](cnf => if (args.contains(long) || args.contains(short)) update(cnf) else cnf)
    } yield ()

    def tryParseMode(long: String, short: String, mode: ExecutionMode): State[Config, Unit] = for {
      cnf <- get[Config]
      file = parseSwitchWithArg(long, short, "")
      _ <- put(if (!file.isEmpty) cnf.copy(mode = mode, oFile = Some(file)) else cnf)
    } yield ()

    for {
    /*
       * Parse mode
       */
      _ <- parseSwitch("--help", "-h", _.copy(mode = Help()))
      _ <- tryParseMode("--batch", "-b", Batch())
      _ <- tryParseMode("--refine", "-r", Refine())
      _ <- tryParseMode("--decide", "-d", Decide())
      _ <- tryParseMode("--show", "-s", Show())
      _ <- tryParseMode("--unfold", "-u", Unfold())
      _ <- tryParseMode("--analyze", "-a", Analyze())
      mode <- gets[Config,ExecutionMode](_.mode)

      /*
       * Parse other args
       */
      // Prop
      propertyString = parseSwitchWithArg("--prop", "-p", "")
      prop = AutomatonTask.fromString(propertyString)
      _ <- modify[Config](cnf => cnf.copy(oProp = prop))

      // Timeout
      timeoutString = parseSwitchWithArg("--timeout", "-t", "")
      timeout = tryParseAsInt(timeoutString) map (Duration(_, SECONDS)) getOrElse {
        if (mode.defaultTimeout.toSeconds != 0) {
          println("Could not parse argument to --timeout; will use default " + mode.defaultTimeout + ". Please pass a positive integer (denoting the timeout in seconds)")
        }
        mode.defaultTimeout
      }
      _ <- modify[Config](cnf => cnf.copy(oTimeout = Some(timeout)))

      // Unfolding depth
      unfoldingString = parseSwitchWithArg("--depth", "-d", "")
      unfoldingDepth = tryParseAsInt(unfoldingString)
      _ <- modify[Config](cnf => cnf.copy(oUnfoldingDepth = unfoldingDepth))

      // Boolean flags
      _ <- parseSwitch("--reduced", "-red", _.copy(oUnfoldingsReduced = Some(true)))
      _ <- parseSwitch("--verbose", "-v", _.copy(verbose = true))
      _ <- parseSwitch("--showprogress", "-sp", _.copy(reportProgress = true))
    } yield ()
  }

  /**
    * Run Harrsh according to the given config
    * @param config Configuration specifying what to run and how to run it
    */
  private def run(config : Config) : Unit = config.mode match {
      case Help() =>
        printUsage()

      case Decide() =>
        // Decision procedure mode
        val task = TaskConfig(config.file, config.prop, None)
        val result = DecisionProcedures.decideInstance(task, config.timeout, config.verbose, config.reportProgress)
        MainIO.printAnalysisResult(task, result)

      case Refine() =>
        // Refinement mode
        println("Will refine SID definition in file " + config.file + " by " + config.prop)
        val sid = RefinementAlgorithms.refineSID(config.file, config.prop, config.timeout, reportProgress = config.reportProgress)
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

      case Batch() =>
          // Batch mode
          println("Will run all benchmarks in " + config.file)
          val tasks = MainIO.readTasksFromFile(config.file)
          val (results, stats) = DecisionProcedures.decideInstances(tasks, config.timeout, config.verbose, config.reportProgress)

          MainIO.writeBenchmarkFile(results, "previous-batch.bms")
          MainIO.printAnalysisResults(results, stats)

          val diffs = DecisionProcedures.deviationsFromExpectations(results)
          if (diffs.nonEmpty) {
            println()
            IOUtils.printWarningToConsole("Some analysis results differed from the expected results as specified in " + config.file)
            for {
              (taskConfig, result) <- diffs
            } IOUtils.printWarningToConsole(taskConfig.fileName + " " + taskConfig.decisionProblem + ": Expected " + taskConfig.expectedResult.get + ", actual " + !result.isEmpty)
          }

      case Show() =>
          // Print mode
          val (sid, _) = MainIO.getSidFromFile(config.file)
          println(sid)
          IOUtils.writeFile(PreviousSidFileName, SID.toHarrshFormat(sid))

      case Unfold() =>
          // Unfold mode
          val (sid, _) = MainIO.getSidFromFile(config.file)
          println(SID.unfold(sid, config.unfoldingDepth, config.unfoldingsReduced).mkString("\n"))

      case Analyze() =>
          val (sid, numfv) = MainIO.getSidFromFile(config.file)
          println(RefinementAlgorithms.performFullAnalysis(sid, numfv, config.timeout))
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
