package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RuleBody, Sid}
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

object EntailmentResultToLatex {

  val decompToLatex: ToLatex[ContextDecomposition] = (a: ContextDecomposition, _: Naming) => decompositionToLatexLines(a).mkString("\n")

  object decompositionToLatexLines {

    // TODO: Less hacky latex conversion

    def apply(decomp: ContextDecomposition): Stream[String] = {
      val ordered = decomp.parts.toStream

      // FIXME: Include pure constraints in latex (which used to be per context, but are now per decomposition, so we need to make some changes about node id, positioning etc)
      //val diseqId = ctxId + "_diseqs"
      //val diseqsTikz = pureConstraintToTikz(decomp.pureConstraints, rootId, diseqId, s"right=of $rootId")
      val tifpics = ordered.zipWithIndex.flatMap {
        case (ctx, ix) =>
          val style = if (ix == 0) "" else {
            val bottomLeft = if (ordered(ix - 1).calls.nonEmpty) s"ctx${ix-1}_0" else s"ctx${ix-1}_root"
              s"below=5mm of $bottomLeft"
          }
          contextToLatexLines(ctx, decomp.constraints.usage, "ctx" + ix, style)
      }
      inTikzPic(tifpics, Some(DecompStyleClass))
    }

    def inTikzPic(lines: Stream[String], style: Option[String] = None): Stream[String] = {
      val styleString = style.map('[' + _ + ']').getOrElse("")
      Stream(s"\\begin{tikzpicture}$styleString") ++ lines ++ Stream("\\end{tikzpicture}")
    }

    def contextToLatexLines(ctx: EntailmentContext, usageInfo: VarUsageByLabel, ctxId: String, rootStyle: String): Stream[String] = {
      val rootId = ctxId + "_root"
      val fitId = ctxId + "_fit"

      val EntailmentContext(root, leaves) = ctx
      val rootTikz = nodeLabelToLatexLines(root, usageInfo, rootId, rootStyle)


      val callsTikz = leaves.toStream.zipWithIndex.flatMap {
        case (leaf, ix) =>
          val position = if (ix == 0) s"below=2mm of $rootId" else s"right=of ${ctxId}_${ix-1}"
          nodeLabelToLatexLines(leaf, usageInfo, ctxId + "_" + ix, s"missing,$position")
      }
      val callIds = (0 until leaves.size) map (ctxId + "_" + _)
      val nodeIds = Seq(rootId) ++ callIds
      val fitConstraint = nodeIds.map("(" + _ + ")").mkString(" ")

      val fitTikz = Stream(s"\\node[draw=black!50, fit={$fitConstraint}] ($fitId) {};")

      rootTikz ++ callsTikz ++ fitTikz
    }

    private val varsToMath = (v: Var) => Naming.indexify(Naming.DefaultNaming)(v) match {
      case "null" => "\\nil"
      case other => other
    }

//    private def pureConstraintToTikz(pureConstraints: PureConstraintTracker, ctxRootId: String, nodeId: String, style: String): Option[String] = {
//      val pureAtomsToLatex = (deqs: Set[PureAtom]) =>
//        if (deqs.isEmpty) {
//          "---"
//        } else {
//          deqs.map(a => s"$$${varsToMath(a.l)} ${if (a.isEquality) "=" else "\\neq"} ${varsToMath(a.r)}$$").mkString(", ")
//        }
//
//      if (pureConstraints.ensured.nonEmpty || pureConstraints.missing.nonEmpty) {
//        val ensuredLabel = s"Guaranteed: ${pureAtomsToLatex(pureConstraints.ensured)}"
//        val missingLabel = s"Missing: ${pureAtomsToLatex(pureConstraints.missing)}"
//        val missingStyle = if (pureConstraints.missing.nonEmpty) s"$MissingStyleClass," else ""
//        Some(s"\\node[$NodeLabelStyleClass,$style,${missingStyle}right=2mm of $ctxRootId] ($nodeId) {\\footnotesize $ensuredLabel \\nodepart{two} \\footnotesize $missingLabel};")
//      } else {
//        None
//      }
//    }

    private def nodeLabelToLatexLines(nodeLabel: ContextPredCall, usageInfo: VarUsageByLabel, nodeId: String, style: String): Stream[String] = {
      val tikzNodeLabel = nodeLabel.symbolicHeapLabel
      val annotateWithUsageInfo = (vs: Set[Var]) => {
        usageInfo(vs) match {
          case VarUnused => ""
          case VarAllocated => "\\overset{\\rightsquigarrow}"
          case VarReferenced => "\\overset{\\leftsquigarrow}"
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

  object entailmentCheckingResultToLatex {

    def apply(ei: EntailmentInstance, holds: Boolean, aut: EntailmentAutomaton, statesByPred: Map[String, Set[EntailmentProfile]], transitions: Map[String, Set[(Seq[EntailmentProfile], RuleBody, EntailmentProfile)]]): String = {
      val resultTex = entailmentCheckerResultToLatex(aut, statesByPred)
      val queryTex = s"$$${ei.lhs.topLevelConstraint.toSymbolicHeap.toLatex} \\models ${ei.rhs.topLevelConstraint.toSymbolicHeap.toLatex}$$"
      val combinedSid = Sid(startPred = "", description = "", preds = ei.lhs.sid.preds ++ ei.rhs.sid.preds.filterNot(ei.lhs.sid.preds.contains))
      val sidTex = combinedSid.toLatex
      latexTemplate.replace(ResultPlaceholder, resultTex)
        .replace(QueryPlaceholder, queryTex)
        .replace(SidPlaceholder, sidTex)
        .replace(ExpectationPlaceholder, ei.entailmentHolds.map(_.toString).getOrElse("unspecified"))
        .replace(EntailmentHoldsPlaceholder, holds.toString)
        .replace(TransitionPlaceholder, transitionsToLatex(transitions))
    }

    private def transitionsToLatex(transitions: Map[String, Set[(Seq[EntailmentProfile], RuleBody, EntailmentProfile)]]): String = {
      val byPred = for {
        (pred, ts) <- transitions
        predStr = s"\\subsection{Transitions for \\texttt{$pred}}\n\\begin{itemize}\n"
        endStr = "\\end{itemize}\n"
      } yield predStr + ts.map(transitionToLatex).map("\\item Transition: " + _).mkString("\n\n") + endStr
      byPred.mkString("\n\n")
    }

    private def transitionToLatex(transition: (Seq[EntailmentProfile], RuleBody, EntailmentProfile)) : String = {
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

    def apply(aut: EntailmentAutomaton, statesByPred: Map[String, Set[EntailmentProfile]]): String = {
      val isFinal = (s: EntailmentProfile) => aut.isFinal(s)
      statesToLatex(statesByPred, isFinal)
    }

    def statesToLatex(statesByPred: Map[String, Set[EntailmentProfile]], isFinal: EntailmentProfile => Boolean): String = {
      val lines = Stream("\\begin{itemize}") ++ statesByPred.toStream.flatMap(pair => Stream("\\item") ++ predToLatex(pair._1, pair._2, isFinal)).map(indent) ++ Stream("\\end{itemize}")
      lines.mkString("\n")
    }

    def predToLatex(pred: String, states: Set[EntailmentProfile], isFinal: EntailmentProfile => Boolean): Stream[String] = {
      Stream(s"Reachable states for \\texttt{$pred}:", "\\begin{itemize}") ++ states.toStream.flatMap(s => stateToLatex(s, isFinal)).map(indent) ++ Stream("\\end{itemize}")
    }

    def stateToLatex(state: EntailmentProfile, isFinal: EntailmentProfile => Boolean): Stream[String] = {
      val finalStr = if (isFinal(state)) "\\textbf{FINAL} " else ""
      val header = s"\\item ${finalStr}State/Profile with free variables ${state.orderedParams.mkString("$<", ", ", ">$")} and context decompositions:"

      val decompsStream = if (state.decompsOrEmptySet.isEmpty) {
        Stream("\\item No consistent context decomposition (failure state)")
      } else {
        Stream("\\item Decompositions:", "\\begin{enumerate}") ++ state.decompsOrEmptySet.toStream.flatMap(decomp => Stream("\\item") ++ decompositionToLatexLines(decomp)) ++ Stream("\\end{enumerate}")
      }

      Stream(header, "\\begin{itemize}") ++ decompsStream ++ Stream("\\end{itemize}")
    }

    private def indent(s : String) = "  " + s

  }

  private val DecompStyleClass = "decomp"
  private val NodeLabelStyleClass = "pcall"
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
                                |\\newcommand{\\RuleName}[1]{\\mathtt{#1}}
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
                                |\\tikzset{$DecompStyleClass/.style={background rectangle/.style={fill=orange!30}, show background rectangle}}
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
