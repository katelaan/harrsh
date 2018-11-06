package at.forsyte.harrsh.main

import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.modelchecking.GreedyUnfoldingModelChecker
import at.forsyte.harrsh.parsers.{EntailmentParsers, slcomp}
import at.forsyte.harrsh.refinement.{AutomatonTask, DecisionProcedures, RefinementAlgorithms}
import at.forsyte.harrsh.seplog.inductive.SIDUnfolding
import at.forsyte.harrsh.util.{Combinators, IOUtils}
import at.forsyte.harrsh.main.ExecutionMode._
import at.forsyte.harrsh.Implicits._

import scala.concurrent.duration.{Duration, SECONDS}
import scalaz.State
import scalaz.State._

/**
  * The main command-line interface of Harrsh.
  */
object Harrsh {
  // TODO Option to feed in arbitrary symbolic heaps in addition to SID (and then automatically adapt the SID with new start predicate)

  val PreviousSidFileName = "result.sid"

  val S = scalaz.StateT.stateMonad[Config]

  def main(args : Array[String]) : Unit = {

    val config: Config = parseAll(args)
    //println(config)

    // Run in specified mode unless something is missing from the config
    if (config.mode != Help && config.oFile.isEmpty) {
      println("No file specified => Terminating")
    }
    else if (config.mode.requiresProp && config.oProp.isEmpty) {
      println("No (valid) property specified => Terminating")
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
      _ <- parseSwitch("--help", "-h", _.copy(mode = Help))
      _ <- tryParseMode("--batch", "-b", RefinementBatch)
      _ <- tryParseMode("--ebatch", "-eb", EntailmentBatch)
      _ <- tryParseMode("--refine", "-r", Refine)
      _ <- tryParseMode("--model", "-m", GetModel)
      _ <- tryParseMode("--decide", "-d", Decide)
      _ <- tryParseMode("--show", "--show", Show)
      _ <- tryParseMode("--unfold", "-u", Unfold)
      _ <- tryParseMode("--analyze", "-a", Analyze)
      _ <- tryParseMode("--entailment", "-e", Entailment)
      _ <- tryParseMode("--spec", "-s", ModelChecking)
      _ <- tryParseMode("--parse", "--parse", ParseOnly)
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
          println("No timeout specified; will use default " + mode.defaultTimeout)
          mode.defaultTimeout
        } else {
          Duration(Integer.MAX_VALUE, SECONDS)
        }
      }
      _ <- modify[Config](cnf => cnf.copy(oTimeout = Some(timeout)))

      // Unfolding depth
      unfoldingString = parseSwitchWithArg("--depth", "-d", "")
      unfoldingDepth = tryParseAsInt(unfoldingString)
      _ <- modify[Config](cnf => cnf.copy(oUnfoldingDepth = unfoldingDepth))

      // Num. FV for entailment automaton
      numfvString = parseSwitchWithArg("--numfv", "-n", "")
      numfv = tryParseAsInt(numfvString)
      _ <- modify[Config](cnf => cnf.copy(oNumFV = numfv))

      // Model
      modelFile = parseSwitchWithArg("--modelcheck", "-mc", "")
      _ <- modify[Config](cnf => cnf.copy(oModelFile = Some(modelFile)))

      // Boolean flags
      _ <- parseSwitch("--reduced", "-red", _.copy(oUnfoldingsReduced = Some(true)))
      _ <- parseSwitch("--verbose", "-v", _.copy(verbose = true))
      _ <- parseSwitch("--showprogress", "-sp", _.copy(reportProgress = true))
      _ <- parseSwitch("--debug", "--debug", _.copy(debug = true))
      _ <- parseSwitch("--disable-per-side-sids", "--no-per-side-sids", _.copy(computeSidsForEachSideOfEntailment = false))
    } yield ()
  }

  /**
    * Run Harrsh according to the given config
    * @param config Configuration specifying what to run and how to run it
    */
  private def run(config : Config) : Unit = config.mode match {
      case Help =>
        printUsage()

      case ParseOnly =>
        println(slcomp.parseFileToSatBenchmark(config.file))

      case Entailment =>
        val fileContent = IOUtils.readFile(config.file)
        EntailmentParsers.parse(fileContent, config.computeSidsForEachSideOfEntailment) match {
          case Some(entailmentInstance) =>
            val res = EntailmentChecker.solve(entailmentInstance)
            println(if (res) "The entailment holds" else "The entailment does NOT hold")
          case None =>
            println("Parsing the entailment input failed.")
        }

      case Decide =>
        val task = TaskConfig(config.file, config.prop, None)
        val result = DecisionProcedures.decideInstance(task, config.timeout, config.verbose, config.reportProgress)
        MainIO.printAnalysisResult(task, result)

      case Refine =>
        println("Will refine SID definition in file " + config.file + " by " + config.prop)
        val (sid, ha) = MainIO.getSidAndAutomaton(config.file, config.prop)
        val result = RefinementAlgorithms.refineSID(sid, ha, config.timeout, reportProgress = config.reportProgress)

        result match {
          case Some(vsid) =>
            println(vsid._1)
            IOUtils.writeFile(PreviousSidFileName, vsid._1.toHarrshFormat)
            println(s"Result exported to $PreviousSidFileName.")

            if (vsid._2) {
              IOUtils.printWarningToConsole(s"Language of refined SID is empty (no rules for start predicate '${vsid._1.startPred}').")
            }
          case None =>
            IOUtils.printWarningToConsole("Refinement failed.")
        }

      case RefinementBatch =>
        println("Will run all refinement benchmarks in " + config.file)
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

      case EntailmentBatch =>
        println("Will run all entailment benchmarks in " + config.file)
        EntailmentBatchMode.runAllEntailmentsInPath(config.file, config.timeout)

      case Show =>
        val sid = MainIO.getSidFromFile(config.file)
        println(sid)
        IOUtils.writeFile(PreviousSidFileName, sid.toHarrshFormat)

      case Unfold =>
        val sid = MainIO.getSidFromFile(config.file)
        println(SIDUnfolding.unfold(sid, config.unfoldingDepth, config.unfoldingsReduced).mkString("\n"))

      case Analyze =>
        val sid = MainIO.getSidFromFile(config.file)
        RefinementAlgorithms.performFullAnalysis(sid, config.timeout, config.verbose)

      case GetModel =>
        println("Will compute model of SID in file " + config.file + " that satisfies property " + config.prop)
        val (sid, ha) = MainIO.getSidAndAutomaton(config.file, config.prop)
        val result = RefinementAlgorithms.refineSID(sid, ha, config.timeout, reportProgress = config.reportProgress)

        result match {
          case Some(vsid) =>
            vsid._1.getModel match {
              case Some(model) => println(model)
              case None => IOUtils.printWarningToConsole(s"No model of the SID satisfies ${config.prop}")
            }
          case None =>
            IOUtils.printWarningToConsole("Refinement failed => Can't get model.")
        }

      case ModelChecking =>
          val sid = MainIO.getSidFromFile(config.file)
          val model = MainIO.getModelFromFile(config.modelFile)
          val modelChecker = GreedyUnfoldingModelChecker
          val result = modelChecker.isModel(model, sid)
          println("Finished model checking. Result: " + result)
  }


  private def tryParseAsInt(s : String) : Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch {
      case _ : Throwable => None
    }
  }

  private def printUsage() = {
    val helpMsg = """This is HARRSH. Usage:
      |
      |Refinement mode:
      |  --refine <relative-path-to-sid-file> --prop <property>      refine sid by prop
      |
      |Decision procedure mode:
      |  --decide <relative-path-to-sid-file> --prop <property>   check if sid has prop
      |
      |Batch / benchmarking mode:
      |  --batch <relative-path-to-file-with-list-of-tasks>          batch benchmarking
      |
      |Analysis mode:
      | --analyze <relative-path-to-sid-file>           analyze robustness of given sid
      |
      |Exploration mode:
      |  --show <relative-path-to-sid-file>                        print sid to std out
      |  --model <relative-path-to-sid-file> --prop <property>
      |                                                  get model of sid that has prop
      |  --unfold <relative-path-to-sid-file>     generate all unfoldings of the sid...
      |     [--depth <depth>]                    ...up to depth <depth> (default: 3)...
      |     [--reduced]                          ...showing only reduced symbolic heaps
      |
      |Optional arguments:
      |  --timeout <timeout in s                                       optional timeout
      |  --showprogress                                    print progress of refinement
      |  --verbose                                                  produce more output
      |
      |Properties:
      |SAT                    Satisfiable unfoldings
      |UNSAT                  Unsatisfiable unfoldings
      |EST                    Established unfoldings
      |NON-EST                Non-established unfoldings
      |GF                     Garbage-free unfoldings
      |GARB                   Unfoldings that may contain garbage
      |ACYC                   Weakly acyclic unfoldings
      |CYC                    Strongly cyclic unfoldings
      |REACH[<var1>,<var2>]   Unfs. in which there must be a path from var1 to var2
      |ALLOC[<vars>]          Unfs. in which at least the given <vars> are allocated
      |PURE[<eqs>]            Unfs. in which at least the given pure constraints hold
      |REL-TR[<vars>:<eqs>]   Unfs. that satify both ALLOC[<vars>] and PURE[<eqs>]
      |TRACK[<vars>:<eqs>]    Unfs. in which EXACTLY the given constraints hold
      |HASPTR                 Unfoldings that allocate memory
      |MOD[n,d]               Unfoldings that allocate == " + "n mod d pointers
      |
      |where (without ANY whitespace!)
      |  <var>   ==  null | x1 | x2 | x3 | ...
      |  <vars>  ==  comma-separated list of <var>
      |  <eq>    ==  <var>=<var> | <var> != <var>
      |  <eqs>   ==  comma-separated list of <eq>""".stripMargin

    println(helpMsg)

    //    Model checking mode:
    //      --modelcheck <path-to-model> --spec <path-to-sid>       check if model |= spec
  }

}
