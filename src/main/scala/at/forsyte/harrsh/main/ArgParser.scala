package at.forsyte.harrsh.main

import at.forsyte.harrsh.main.ExecutionMode._
import GlobalConfig.params

object ArgParser {

  def apply(args: Array[String]): (ExecutionMode, String) = {
    if (args.length < 2) {
      (Help, "")
    } else {
      val modeArg = args(0)
      val mode = findMode(args(0)).getOrElse(Help)
      GlobalConfig.addKeyValuePairsToConfig(args.drop(2))
      (mode, args(1))
    }
  }

  private def findMode(arg: String): Option[ExecutionMode] = {
    for {
      modeDef <- modes.find(m => m.long == arg || m.short.contains(arg))
    } yield modeDef.mode
  }

  private case class CommandLineOption(flag: String, value: String, description: Option[String], inHelpMessage: Boolean) {
    def mainStr: String = s"$flag=<$value>"

    override def toString: String = {
      val fst = mainStr
      val snd = description.getOrElse("")
      val padding = " " * (76-fst.length-snd.length)
      "    " + fst + padding + snd
    }
  }

  private case class ModeDef(mode: ExecutionMode, long: String, short: Option[String], fileDesc: String, description: String, inHelpMessage: Boolean, mandatoryOptions: Seq[CommandLineOption], otherOptions: Seq[CommandLineOption]) {

    override def toString: String = {
      val flagStr = ("  "+long+short.map(", "+_).getOrElse("")).padTo(20, ' ')
      val options = mandatoryOptions ++ otherOptions
      val optionsStr = if (options.nonEmpty) options.mkString("\n","\n","") else ""
      flagStr + description + optionsStr
    }

  }

  // TODO: Add all the remaining options (including general options such as timeout, verbosity etc.)
  private val propOption = CommandLineOption(params.Property, "property (default: SAT)", None, inHelpMessage = true)
  private val modelOption = CommandLineOption(params.ModelFile, "file", None, inHelpMessage = true)
  private val unfoldingsReduced = CommandLineOption(params.UnfoldingsReduced, "bool", Some("Only generate reduced unfoldings"), inHelpMessage = true)
  private val unfoldingDepth = CommandLineOption(params.UnfoldingDepth, "int", Some("Max. depth of generated unfoldings"), inHelpMessage = true)

  private val startEntailmentWithSatCheckOption = CommandLineOption(params.StartEntailmentWithSatCheck, "bool", Some("First check if LHS is UNSAT (and the entailment thus valid) (default: false)"), inHelpMessage = true)
  private val withPatternMatchingStageOption = CommandLineOption(params.PatternMatchingLevel, "0..3", Some("Try to solve with pattern matching; 0: inactive,...,3: advanced (default: 3)"), inHelpMessage = true)
  private val computePerSideSidsOption = CommandLineOption(params.ComputePerSideSids, "bool", Some("Optimize fixed-point for the RHS of the query (default: true)"), inHelpMessage = true)
  private val computeSccsOption = CommandLineOption(params.ComputeSccs, "bool", Some("Try to split top-level query based on reachability (default: false"), inHelpMessage = false)
  private val useUnionSolverOption = CommandLineOption(params.UseUnionSolver, "bool", Some("Solve big queries with union profiles (default: false"), inHelpMessage = false)

  val UseUnionSolver = "union-solver"

  private val modes = Seq(
    ModeDef(Entailment, "--entailment", Some("-e"), "file", "Entailment checking", inHelpMessage = true, Seq.empty,
      Seq(startEntailmentWithSatCheckOption,withPatternMatchingStageOption,computePerSideSidsOption,computeSccsOption,useUnionSolverOption)),
    ModeDef(Decide, "--decide", Some("-d"), "file", "Check if the given SID has the given property", inHelpMessage = true, Seq(propOption), Seq.empty),
    ModeDef(Refine, "--refine", Some("-r"), "file", "Refine the given SID by the given property", inHelpMessage = true, Seq(propOption), Seq.empty),
    ModeDef(Analyze, "--analyze", Some("-a"), "file", "Analyze Robustness of given SID", inHelpMessage = true, Seq.empty, Seq.empty),
    ModeDef(EntailmentBatch, "--ebatch", Some("-eb"), "path-to-directory", "Entailment batch mode", inHelpMessage = true, Seq.empty, Seq.empty),
    ModeDef(ConvertEntailmentBatch, "--convert", None, "path-to-directory", "Convert files in path to all supported format", inHelpMessage = true, Seq.empty, Seq.empty),
    ModeDef(RefinementBatch, "--rbatch", Some("-rb"), "file-with-list-of-tasks", "Refinement batch mode", inHelpMessage = true, Seq.empty, Seq.empty),
    ModeDef(GetModel, "--model", Some("-m"), "file", "Get model of the given SID", inHelpMessage = true, Seq.empty, Seq.empty),
    ModeDef(Unfold, "--unfold", Some("-u"), "file", "Unfold the given SID", inHelpMessage = true, Seq.empty, Seq(unfoldingsReduced, unfoldingDepth)),
    ModeDef(ParseOnly, "--parse", Some("-p"), "file", "Parse only", inHelpMessage = true, Seq.empty, Seq.empty),
    ModeDef(Show, "--show", None, "file", "Print SID in Harrsh format", inHelpMessage = false, Seq.empty, Seq.empty),
    ModeDef(ModelChecking, "--spec", Some("-s"), "", "", inHelpMessage = false, Seq(modelOption), Seq.empty),
    ModeDef(TacasArtifact, "--tacas", None, "", "", inHelpMessage = false, Seq.empty, Seq.empty)
  )

  val propertiesString =
    """Properties:
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

  def usageMsg(): String = {
    s"This is HARRSH. Usage:\nharrsh <mode> <file> [options]\n\nSupported modes:\n\n${modes.filter(_.inHelpMessage).mkString("\n")}\n\n$propertiesString"
  }
}
