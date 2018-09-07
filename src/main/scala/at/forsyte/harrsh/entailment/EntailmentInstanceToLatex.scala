package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentInstance
import at.forsyte.harrsh.entailment.VarUsage.{Allocated, Referenced, Unused}
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RuleBody, SID}
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

object EntailmentInstanceToLatex {

  val etypeToLatex: ToLatex[ExtensionType] = (a: ExtensionType, _: Naming) => extensionTypeToLatexLines(a).mkString("\n")

  object extensionTypeToLatexLines {

    // TODO: Less hacky latex conversion
    // TODO: Reduce code duplication with ForestsToLatex

    def apply(extensionType: ExtensionType): Stream[String] = {
      val ordered = extensionType.parts.toStream
      val tifpics = ordered.zipWithIndex.flatMap {
        case (tif, ix) =>
          val style = if (ix == 0) "" else {
            val bottomLeft = if (ordered(ix - 1).leaves.nonEmpty) s"tif${ix-1}_0" else s"tif${ix-1}_root"
              s"below=5mm of $bottomLeft"
          }
          treeInterfaceToLatexLines(tif, "tif" + ix, style)
      }
      inTikzPic(tifpics, Some(EtStyleClass))
    }

    def inTikzPic(lines: Stream[String], style: Option[String] = None): Stream[String] = {
      val styleString = style.map('[' + _ + ']').getOrElse("")
      Stream(s"\\begin{tikzpicture}$styleString") ++ lines ++ Stream("\\end{tikzpicture}")
    }

    def treeInterfaceToLatexLines(tif: TreeInterface, tifId: String, rootStyle: String): Stream[String] = {
      val rootId = tifId + "_root"
      val diseqId = tifId + "_diseqs"
      val fitId = tifId + "_fit"

      val TreeInterface(root, leaves, usageInfo, diseqs) = tif
      val rootTikz = nodeLabelToLatexLines(root, usageInfo, rootId, rootStyle)
      val diseqsTikz = pureConstraintToTikz(diseqs, rootId, diseqId, s"right=of $rootId")

      val leavesTikz = leaves.toStream.zipWithIndex.flatMap {
        case (leaf, ix) =>
          val position = if (ix == 0) s"below=2mm of $rootId" else s"right=of ${tifId}_${ix-1}"
          nodeLabelToLatexLines(leaf, usageInfo, tifId + "_" + ix, s"missing,$position")
      }
      val leafIds = (0 until leaves.size) map (tifId + "_" + _)
      val nodeIds = Seq(rootId, diseqId) ++ leafIds
      val fitConstraint = nodeIds.map("(" + _ + ")").mkString(" ")

      val fitTikz = Stream(s"\\node[draw=black!50, fit={$fitConstraint}] ($fitId) {};")

      rootTikz ++ diseqsTikz ++ leavesTikz ++ fitTikz
    }

    private val varsToMath = (v: Var) => Naming.indexify(Naming.DefaultNaming)(v) match {
      case "null" => "\\nil"
      case other => other
    }

    private def pureConstraintToTikz(pureConstraints: PureConstraintTracker, tifRootId: String, nodeId: String, style: String): Stream[String] = {
      val pureAtomsToLatex = (deqs: Set[PureAtom]) =>
        if (deqs.isEmpty) {
          "---"
        } else {
          deqs.map(a => s"$$${varsToMath(a.l)} ${if (a.isEquality) "=" else "\\neq"} ${varsToMath(a.r)}$$").mkString(", ")
        }

      if (pureConstraints.ensured.nonEmpty || pureConstraints.missing.nonEmpty) {
        val ensuredLabel = s"Guaranteed: ${pureAtomsToLatex(pureConstraints.ensured)}"
        val missingLabel = s"Missing: ${pureAtomsToLatex(pureConstraints.missing)}"
        val missingStyle = if (pureConstraints.missing.nonEmpty) s"$MissingStyleClass," else ""
        Stream(s"\\node[$NodeLabelStyleClass,$style,${missingStyle}right=2mm of $tifRootId] ($nodeId) {\\footnotesize $ensuredLabel \\nodepart{two} \\footnotesize $missingLabel};")
      } else {
        Stream.empty
      }
    }

    private def nodeLabelToLatexLines(nodeLabel: NodeLabel, usageInfo: VarUsageByLabel, nodeId: String, style: String): Stream[String] = {
      val tikzNodeLabel = nodeLabel match {
        case _:RuleNodeLabel => nodeLabel.pred.head + ": " + nodeLabel.symbolicHeapLabel
        case _:AbstractLeafNodeLabel => nodeLabel.symbolicHeapLabel
      }
      val annotateWithUsageInfo = (vs: Set[Var]) => {
        usageInfo(vs) match {
          case Unused => ""
          case Allocated => "\\overset{\\rightsquigarrow}"
          case Referenced => "\\overset{\\leftsquigarrow}"
        }
      }

      val (pred, subst) = (nodeLabel.pred, nodeLabel.subst)
      val pairs = (pred.params, subst.toSeq).zipped.map {
        case (from, to) => s"${annotateWithUsageInfo(to)}{${varsToMath(from)}}" + " \\rightarrow " + to.map(varsToMath).mkString(",")
      }
      val substLatex = '$' + pairs.mkString(";") + '$'

      Stream(s"\\node[$NodeLabelStyleClass,$style] ($nodeId) {$tikzNodeLabel \\nodepart{two} \\footnotesize $substLatex};")
    }
  }

  object entailmentInstanceToLatex {

    def apply(ei: EntailmentInstance, holds: Boolean, aut: EntailmentAutomaton, statesByPred: Map[String, Set[EntailmentAutomaton.State]], transitions: Map[String, Set[(Seq[EntailmentAutomaton.State], RuleBody, EntailmentAutomaton.State)]]): String = {
      val resultTex = entailmentCheckerResultToLatex(aut, statesByPred)
      val queryTex = s"$$${ei.lhsCall.toSymbolicHeap.toLatex} \\models ${ei.rhsCall.toSymbolicHeap.toLatex}$$"
      val combinedSid = SID(startPred = "", description = "", preds = ei.lhsSid.preds ++ ei.rhsSid.preds.filterNot(ei.lhsSid.preds.contains))
      val sidTex = combinedSid.toLatex
      latexTemplate.replace(ResultPlaceholder, resultTex)
        .replace(QueryPlaceholder, queryTex)
        .replace(SidPlaceholder, sidTex)
        .replace(ExpectationPlaceholder, ei.entailmentHolds.map(_.toString).getOrElse("unspecified"))
        .replace(EntailmentHoldsPlaceholder, holds.toString)
        .replace(TransitionPlaceholder, transitionsToLatex(transitions))
    }

    private def transitionsToLatex(transitions: Map[String, Set[(Seq[EntailmentAutomaton.State], RuleBody, EntailmentAutomaton.State)]]): String = {
      val byPred = for {
        (pred, ts) <- transitions
        predStr = s"\\subsection{Transitions for \\texttt{$pred}}\n\\begin{itemize}\n"
        endStr = "\\end{itemize}\n"
      } yield predStr + ts.map(transitionToLatex).map("\\item Transition: " + _).mkString("\n\n") + endStr
      byPred.mkString("\n\n")
    }

    private def transitionToLatex(transition: (Seq[EntailmentAutomaton.State], RuleBody, EntailmentAutomaton.State)) : String = {
      val (srcs, rule, trg) = transition
      // TODO: Remove code duplication
      val srcStrs = (srcs map (s => entailmentCheckerResultToLatex.stateToLatex(trg, s => false).mkString("\n"))).mkString("\n\n")
      val srcStrInItemize = if (srcStrs.nonEmpty) s"\\begin{itemize}$srcStrs\\end{itemize}" else srcStrs
      val bodyStr = '$' + rule.body.toLatex(rule.naming).replaceAllLiterally("α", """\alpha""") + '$'
      val trgStr = entailmentCheckerResultToLatex.stateToLatex(trg, _ => false).mkString("\n").drop(5)
      s"\\begin{itemize}\\item Source states:\n\n \n$srcStrInItemize\n \\item Rule body: $bodyStr\n \\item Target state:\n $trgStr \\end{itemize}"
    }


  }

  object entailmentCheckerResultToLatex {

    def apply(aut: EntailmentAutomaton, statesByPred: Map[String, Set[EntailmentAutomaton.State]]): String = {
      val isFinal = (s: EntailmentAutomaton.State) => aut.isFinal(s)
      statesToLatex(statesByPred, isFinal)
    }

    def statesToLatex(statesByPred: Map[String, Set[EntailmentAutomaton.State]], isFinal: EntailmentAutomaton.State => Boolean): String = {
      val lines = Stream("\\begin{itemize}") ++ statesByPred.toStream.flatMap(pair => Stream("\\item") ++ predToLatex(pair._1, pair._2, isFinal)).map(indent) ++ Stream("\\end{itemize}")
      lines.mkString("\n")
    }

    def predToLatex(pred: String, states: Set[EntailmentAutomaton.State], isFinal: EntailmentAutomaton.State => Boolean): Stream[String] = {
      Stream(s"Reachable states for \\texttt{$pred}:", "\\begin{itemize}") ++ states.toStream.flatMap(s => stateToLatex(s, isFinal)).map(indent) ++ Stream("\\end{itemize}")
    }

    def stateToLatex(state: EntailmentAutomaton.State, isFinal: EntailmentAutomaton.State => Boolean): Stream[String] = {
      val finalStr = if (isFinal(state)) "\\textbf{FINAL} " else ""
      val header = s"\\item ${finalStr}State with params ${state.orderedParams.mkString(", ")} and extension types:"

      val etsStream = if (state.ets.isEmpty) {
        Stream("\\item No consistent extension type (failure state)")
      } else {
        Stream("\\item Extension types:", "\\begin{enumerate}") ++ state.ets.toStream.flatMap(et => Stream("\\item") ++ extensionTypeToLatexLines(et)) ++ Stream("\\end{enumerate}")
      }

      Stream(header, "\\begin{itemize}") ++ etsStream ++ Stream("\\end{itemize}")
    }

    private def indent(s : String) = "  " + s

  }

  private val EtStyleClass = "et"
  private val NodeLabelStyleClass = "utnode"
  private val MissingStyleClass = "missing"
  private val SidPlaceholder = "SIDPLACEHOLDER"
  private val QueryPlaceholder = "QUERYPLACEHOLDER"
  private val ResultPlaceholder = "RESULTPLACEHOLDER"
  private val ExpectationPlaceholder = "EXPECTATIONPLACEHOLDER"
  private val EntailmentHoldsPlaceholder = "EHPLACEHOLDER"
  private val TransitionPlaceholder = "TRANSPLACEHOLDER"

  private val latexTemplate = s"""\\documentclass{article}
                                |\\usepackage[a2paper, landscape]{geometry}
                                |\\usepackage{amsmath,amssymb}
                                |
                                |\\newcommand{\\nil}{\\ensuremath{\\textnormal{\\textbf{null}}}}
                                |
                                |\\makeatletter
                                |\\providecommand{\\leftsquigarrow}{%
                                |  \\mathrel{\\mathpalette\\reflect@squig\\relax}%
                                |}
                                |\\newcommand{\\reflect@squig}[2]{%
                                |  \\reflectbox{$$\\m@th#1\\rightsquigarrow $$}%
                                |}
                                |\\makeatother
                                |
                                |\\usepackage{tikz}
                                |\\usetikzlibrary{backgrounds,arrows,shapes,positioning,fit}
                                |
                                |\\tikzset{$EtStyleClass/.style={background rectangle/.style={fill=orange!30}, show background rectangle}}
                                |\\tikzset{$NodeLabelStyleClass/.style={draw,rectangle split, rectangle split
                                |    parts=2,inner sep=2pt}}
                                |\\tikzset{$MissingStyleClass/.style={fill=red!30}}
                                |
                                |\\begin{document}
                                |
                                |\\section{Input}
                                |
                                |\\begin{itemize}
                                |\\item Query: $QueryPlaceholder
                                |
                                |\\item SID:
                                |
                                |$SidPlaceholder
                                |
                                |\\item Expected result: $ExpectationPlaceholder
                                |
                                |\\item Actual result: $EntailmentHoldsPlaceholder
                                |\\end{itemize}
                                |
                                |\\section{Result}
                                |
                                |$ResultPlaceholder
                                |
                                |\\section{Transitions}
                                |
                                |$TransitionPlaceholder
                                |
                                |\\end{document}""".stripMargin('|')

}
