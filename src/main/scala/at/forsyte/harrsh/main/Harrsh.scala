package at.forsyte.harrsh.main

import GlobalConfig.params
import at.forsyte.harrsh.Implicits
import at.forsyte.harrsh.converters._
import at.forsyte.harrsh.entailment.{EntailmentChecker, EntailmentConfig}
import at.forsyte.harrsh.modelchecking.GreedyUnfoldingModelChecker
import at.forsyte.harrsh.parsers.QueryParser
import at.forsyte.harrsh.refinement.{DecisionProcedures, RefinementAlgorithms}
import at.forsyte.harrsh.seplog.inductive.SidUnfolding
import at.forsyte.harrsh.util.{Combinators, IOUtils}
import at.forsyte.harrsh.main.ExecutionMode._

import scala.util.{Failure, Success, Try}

/**
  * The main command-line interface of Harrsh.
  */
object Harrsh extends Implicits {
  // TODO Option to feed in arbitrary symbolic heaps in addition to SID (and then automatically adapt the SID with new start predicate)

  val PreviousSidFileName = "result.sid"

  def main(args : Array[String]) : Unit = {

    val (mode, file) = ArgParser(args)

    // Run in specified mode unless something is missing from the config
    if (mode != Help && file == "") {
      println("No file specified => Terminating")
    }
    else if (mode.requiresProp && !GlobalConfig.isDefinedAt(params.Property)) {
      println("No (valid) property specified => Terminating")
    } else {
      Combinators.swallowExceptions[(ExecutionMode,String)](
        pair => run(pair._1, pair._2),
        GlobalConfig.getBoolean(params.Debug))(mode, file)
    }
  }

  /**
    * Run Harrsh in the given mode
    */
  private def run(mode: ExecutionMode, file: String) : Unit = mode match {
    case Help =>
      println(ArgParser.usageMsg())

    case ParseOnly =>
      println(QueryParser(file))

    case Entailment =>
      val entailmentConfig = EntailmentConfig.fromGlobalConfig()
      QueryParser(file).toEntailmentInstance(entailmentConfig.computeSidsForEachSideOfEntailment, entailmentConfig.computeSccsForTopLevelFormulas) match {
        case Success(entailmentInstance) =>
          val res = EntailmentChecker.solve(entailmentInstance, entailmentConfig)
          println(if (res._1) "The entailment holds" else "The entailment does NOT hold")
          println(res._2.prettyPrint)
        case Failure(e) =>
          println("Parsing the entailment input failed with exception: " + e.getMessage)
      }

    case Decide =>
      GlobalConfig.getProp foreach { prop =>
        val task = RefinementQuery(file, prop)
        val result = DecisionProcedures.decideInstance(task,
          GlobalConfig.getTimeoutForCurrentMode,
          GlobalConfig.getBoolean(params.Verbose),
          GlobalConfig.getBoolean(params.ReportProgress))
        MainIO.printAnalysisResult(task, result)
      }


    case Refine =>
      GlobalConfig.getProp foreach { prop =>
        println(s"Will refine SID definition in file $file by $prop")
        val query = RefinementQuery(file, prop)
        val result = RefinementAlgorithms.refineSID(query.sid,
          query.automaton,
          GlobalConfig.getTimeoutForCurrentMode,
          reportProgress = GlobalConfig.getBoolean(params.ReportProgress))

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
      }

    case RefinementBatch =>
      println("Will run all refinement benchmarks in " + file)
      val tasks = MainIO.readTasksFromFile(file)
      val (results, stats) = DecisionProcedures.decideInstances(tasks,
        GlobalConfig.getTimeoutForCurrentMode,
        GlobalConfig.getBoolean(params.Verbose),
        GlobalConfig.getBoolean(params.ReportProgress))

      MainIO.writeBenchmarkFile(results, "previous-batch.bms")
      MainIO.printAnalysisResults(results, stats)

      val diffs = DecisionProcedures.deviationsFromExpectations(results)
      if (diffs.nonEmpty) {
        println()
        IOUtils.printWarningToConsole("Some analysis results differed from the expected results as specified in " + file)
        for {
          (taskConfig, result) <- diffs
        } IOUtils.printWarningToConsole(taskConfig.fileNameString + " " + taskConfig.taskString + ": Expected " + taskConfig.status.toBoolean.get + ", actual result " + !result.isEmpty)
      }
      else {
        println("All results are as expected.")
      }

    case EntailmentBatch =>
      println("Will run all entailment benchmarks in " + file)
      EntailmentBatchMode.runAllEntailmentsInPath(file, GlobalConfig.getTimeoutForCurrentMode)

    case TacasArtifact =>
      println(s"TACAS artifact: Will run experiments specified in '$file'")
      EntailmentBenchmarking.runJmhBenchmarskForHarrsh(file)

    case ConvertEntailmentBatch =>
      val wrapper = (format: String, conv: EntailmentFormatConverter) => {
        println(s"Will convert all benchmarks in $file to $format input format")
        EntailmentBatchMode.convertAllEntailmentsInPath(file, None, conv)
        println("Done.")
      }
      wrapper("SLCOMP", ToSlcompConverter)
      wrapper("SLIDE", ToSlideFormat)
      wrapper("SONGBIRD", ToSongbirdFormat)
      wrapper("LaTeX", ToLatexConverter)

    case Show =>
      val sid = QueryParser.getSidFromFile(file)
      println(sid)
      IOUtils.writeFile(PreviousSidFileName, sid.toHarrshFormat)

    case Unfold =>
      val sid = QueryParser.getSidFromFile(file)
      println(SidUnfolding.unfold(sid,
        GlobalConfig.getInt(params.UnfoldingDepth),
        GlobalConfig.getBoolean(params.UnfoldingsReduced)
      ).mkString("\n"))

    case Analyze =>
      val sid = QueryParser.getSidFromFile(file)
      RefinementAlgorithms.performFullAnalysis(sid, GlobalConfig.getTimeoutForCurrentMode, GlobalConfig.getBoolean(params.Verbose))

    case GetModel =>
      GlobalConfig.getProp foreach { prop =>
        println("Will compute model of SID in file " + file + " that satisfies property " + prop)
        val query = RefinementQuery(file, prop)
        val result = RefinementAlgorithms.refineSID(query.sid,
          query.automaton,
          GlobalConfig.getTimeoutForCurrentMode,
          Try { GlobalConfig.getInt(params.SatCheckingIncrementalFromNumCalls) }.toOption,
          GlobalConfig.getBoolean(params.ReportProgress))

        result match {
          case Some(vsid) =>
            vsid._1.getModel match {
              case Some(model) => println(model)
              case None => IOUtils.printWarningToConsole(s"No model of the SID satisfies $prop")
            }
          case None =>
            IOUtils.printWarningToConsole("Refinement failed => Can't get model.")
        }
      }

    case ModelChecking =>
      Try { GlobalConfig.getString(params.ModelFile) } foreach { modelFile =>
        val sid = QueryParser.getSidFromFile(file)
        val model = MainIO.getModelFromFile(modelFile)
        val modelChecker = GreedyUnfoldingModelChecker
        val result = modelChecker.isModel(model, sid)
        println("Finished model checking. Result: " + result)
      }
  }

}
