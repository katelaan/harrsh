package at.forsyte.harrsh.main

import scala.concurrent.duration.{Duration, SECONDS}
import scalaz.State
import scalaz.State._
import at.forsyte.harrsh.main.ExecutionMode._
import at.forsyte.harrsh.refinement.AutomatonTask

import scala.util.Try

object ArgParser {

  val S = scalaz.StateT.stateMonad[Config]

  /**
    * Parse command line arguments
    * @param args Array of arguments
    * @return Config built from arguments
    */
  def apply(args : Array[String]) : Config = parseAllAux(args).run(Config.DefaultConfig)._1

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
      _ <- tryParseMode("--rbatch", "-rb", RefinementBatch)
      _ <- tryParseMode("--ebatch", "-eb", EntailmentBatch)
      _ <- tryParseMode("--convert", "-ceb", ConvertEntailmentBatch)
      _ <- tryParseMode("--refine", "-r", Refine)
      _ <- tryParseMode("--model", "-m", GetModel)
      _ <- tryParseMode("--decide", "-d", Decide)
      _ <- tryParseMode("--show", "--show", Show)
      _ <- tryParseMode("--unfold", "-u", Unfold)
      _ <- tryParseMode("--analyze", "-a", Analyze)
      _ <- tryParseMode("--entailment", "-e", Entailment)
      _ <- tryParseMode("--spec", "-s", ModelChecking)
      _ <- tryParseMode("--parse", "--parse", ParseOnly)
      _ <- tryParseMode("--tacas", "--tacas", TacasArtifact)
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
      _ <- parseSwitch("--compute-sccs", "-scc", _.copy(computeSccsForTopLevelFormulas = true))
    } yield ()
  }

  private def tryParseAsInt(s : String) : Option[Int] = {
    Try {
      Integer.parseInt(s)
    }.toOption
  }

}
