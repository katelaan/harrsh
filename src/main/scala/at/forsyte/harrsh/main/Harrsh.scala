package at.forsyte.harrsh.main


import at.forsyte.harrsh.Implicits
import at.forsyte.harrsh.converters._
import at.forsyte.harrsh.entailment.EntailmentChecker
import at.forsyte.harrsh.modelchecking.GreedyUnfoldingModelChecker
import at.forsyte.harrsh.parsers.QueryParser
import at.forsyte.harrsh.refinement.{DecisionProcedures, RefinementAlgorithms}
import at.forsyte.harrsh.seplog.inductive.SidUnfolding
import at.forsyte.harrsh.util.{Combinators, IOUtils}
import at.forsyte.harrsh.main.ExecutionMode._

import scala.util.{Failure, Success}

/**
  * The main command-line interface of Harrsh.
  */
object Harrsh extends Implicits {
  // TODO Option to feed in arbitrary symbolic heaps in addition to SID (and then automatically adapt the SID with new start predicate)

  val PreviousSidFileName = "result.sid"

  def main(args : Array[String]) : Unit = {

    val config: Config = ArgParser(args)

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
    * Run Harrsh according to the given config
    * @param config Configuration specifying what to run and how to run it
    */
  private def run(config : Config) : Unit = config.mode match {
      case Help =>
        printUsage()

      case ParseOnly =>
        println(QueryParser(config.file))

      case Entailment =>
        QueryParser(config.file).toEntailmentInstance(config.computeSidsForEachSideOfEntailment) match {
          case Success(entailmentInstance) =>
            val res = EntailmentChecker.solve(entailmentInstance)
            println(if (res._1) "The entailment holds" else "The entailment does NOT hold")
            res._2.foreach(stats => println(stats.prettyPrint))
          case Failure(e) =>
            println("Parsing the entailment input failed with exception: " + e.getMessage)
        }

      case Decide =>
        val task = RefinementQuery(config.file, config.prop)
        val result = DecisionProcedures.decideInstance(task, config.timeout, config.verbose, config.reportProgress)
        MainIO.printAnalysisResult(task, result)

      case Refine =>
        println("Will refine SID definition in file " + config.file + " by " + config.prop)
        val query = RefinementQuery(config.file, config.prop)
        val result = RefinementAlgorithms.refineSID(query.sid, query.automaton, config.timeout, reportProgress = config.reportProgress)

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
          } IOUtils.printWarningToConsole(taskConfig.fileNameString + " " + taskConfig.taskString + ": Expected " + taskConfig.status.toBoolean.get + ", actual result " + !result.isEmpty)
        }
        else {
          println("All results are as expected.")
        }

      case EntailmentBatch =>
        println("Will run all entailment benchmarks in " + config.file)
        EntailmentBatchMode.runAllEntailmentsInPath(config.file, config.timeout)

      case TacasArtifact =>
        println(s"TACAS artifact: Will run experiments specified in '${config.file}'")
        EntailmentBenchmarking.runJmhBenchmarskForHarrsh(config.file)

      case ConvertEntailmentBatch =>
        val wrapper = (format: String, conv: EntailmentFormatConverter) => {
          println(s"Will convert all benchmarks in ${config.file} to $format input format")
          EntailmentBatchMode.convertAllEntailmentsInPath(config.file, None, conv)
          println("Done.")
        }
        wrapper("SLCOMP", ToSlcompConverter)
        wrapper("SLIDE", ToSlideFormat)
        wrapper("SONGBIRD", ToSongbirdFormat)
        wrapper("LaTeX", ToLatexConverter)

      case Show =>
        val sid = QueryParser.getSidFromFile(config.file)
        println(sid)
        IOUtils.writeFile(PreviousSidFileName, sid.toHarrshFormat)

      case Unfold =>
        val sid = QueryParser.getSidFromFile(config.file)
        println(SidUnfolding.unfold(sid, config.unfoldingDepth, config.unfoldingsReduced).mkString("\n"))

      case Analyze =>
        val sid = QueryParser.getSidFromFile(config.file)
        RefinementAlgorithms.performFullAnalysis(sid, config.timeout, config.verbose)

      case GetModel =>
        println("Will compute model of SID in file " + config.file + " that satisfies property " + config.prop)
        val query = RefinementQuery(config.file, config.prop)
        val result = RefinementAlgorithms.refineSID(query.sid, query.automaton, config.timeout, reportProgress = config.reportProgress)

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
        val sid = QueryParser.getSidFromFile(config.file)
        val model = MainIO.getModelFromFile(config.modelFile)
        val modelChecker = GreedyUnfoldingModelChecker
        val result = modelChecker.isModel(model, sid)
        println("Finished model checking. Result: " + result)
  }

  private def printUsage() = {
    val helpMsg = """This is HARRSH. Usage:
      |Entailment checking:
      |  --entailment <relative-path-to-file>                discharge given entailment
      |
      |Refinement:
      |  --refine <relative-path-to-sid-file> --prop <property>      refine sid by prop
      |
      |Decide property:
      |  --decide <relative-path-to-sid-file> --prop <property>   check if sid has prop
      |
      |Batch / benchmarking modes:
      |  --ebatch <relative-path-to-directory>                         entailment batch
      |  --rbatch <relative-path-to-file-with-list-of-tasks>           refinement batch
      |
      |Analysis:
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
      |  --timeout <timeout in s>                                      optional timeout
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
      |  <eq>    ==  <var>=<var> | <var>!=<var>
      |  <eqs>   ==  comma-separated list of <eq>""".stripMargin

    println(helpMsg)

    //    Model checking mode:
    //      --modelcheck <path-to-model> --spec <path-to-sid>       check if model |= spec
  }

}
