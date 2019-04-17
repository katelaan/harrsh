package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.HeapAutomaton.Transition
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.inductive.{Predicate, Sid}
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

object EntailmentResultToLatex {

  val decompToLatex: ToLatex[ContextDecomposition] = (a: ContextDecomposition, _: Naming) => decompositionToLatexLines(a).mkString("\n")

  private def varToMath(v: Var) = v.toLatex(Naming.indexify(Naming.DefaultNaming))

  private def varsToMath(vs: Iterable[Var]) = {
    val mathVars = vs map varToMath
    if (mathVars.size == 1) mathVars.head else mathVars.mkString("\\{", ",", "\\}")
  }

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
      def annotateWithUsageInfo(vs: Set[Var])(latex: String) = {
        val prefix = usageInfo(vs) match {
          case VarUnused => ""
          case VarAllocated => "\\overset{\\rightsquigarrow}"
          case VarReferenced => "\\overset{\\leftsquigarrow}"
        }
        prefix + "{" + latex + "}"
      }

      val (pred, subst) = (nodeLabel.pred, nodeLabel.subst)
      val paramLabels = (pred.params, subst.toSeq).zipped.map {
        case (from, to) => annotateWithUsageInfo(to)(varsToMath(to))
      }
      val tikzNodeLabel = '$' + "\\mathtt{" + pred.headToLatex + "}" + paramLabels.mkString("(", ", ", ")") + '$'
      Stream(s"\\node[$NodeLabelStyleClass,$style] ($nodeId) {$tikzNodeLabel};")
    }
  }

  object entailmentFixedPointToLatex {

    def apply(ei: EntailmentInstance, aut: EntailmentAutomaton, statesByPred: Map[String, Set[EntailmentProfile]], transitions: Map[String, Set[Transition[EntailmentProfile]]]): String = {
      val resultTex = entailmentCheckerResultToLatex(aut, statesByPred)
      val queryTex = s"$$${ei.lhs.topLevelConstraint.toSymbolicHeap.toLatex} \\models ${ei.rhs.topLevelConstraint.toSymbolicHeap.toLatex}$$"
      val combinedSid = Sid(startPred = "", description = "", preds = ei.lhs.sid.preds ++ ei.rhs.sid.preds.filterNot(ei.lhs.sid.preds.contains))
      val sidTex = combinedSid.toLatex
      latexTemplate.replace(ResultPlaceholder, resultTex)
        .replace(QueryPlaceholder, queryTex)
        .replace(SidPlaceholder, sidTex)
        .replace(TransitionPlaceholder, transitionsToLatex(transitions))
    }

    private def transitionsToLatex(transitions: Map[String, Set[Transition[EntailmentProfile]]]): String = {
      val byPred = for {
        (pred, ts) <- transitions
        predStr = s"\\subsection{Transitions for \\texttt{${Predicate.predicateHeadToLatex(pred)}}}\n\\begin{itemize}\n"
        endStr = "\\end{itemize}\n"
      } yield predStr + ts.map(transitionToLatex).map("\\item Transition: " + _).mkString("\n\n") + endStr
      byPred.mkString("\n\n")
    }

    private def transitionToLatex(transition: Transition[EntailmentProfile]) : String = {
      val Transition(srcStates, body, Some(localState), headPredicate, trgState) = transition
      val localStr = entailmentCheckerResultToLatex.stateToLatex(localState).mkString("\n").drop(5)
      val srcStrs = (srcStates map (s => entailmentCheckerResultToLatex.stateToLatex(s).mkString("\n"))).mkString("\n\n")
      val srcStrInItemize = if (srcStrs.nonEmpty) s"\\begin{itemize}$srcStrs\\end{itemize}" else srcStrs
      val bodyStr = '$' + body.toLatex + '$'
      val trgStr = entailmentCheckerResultToLatex.stateToLatex(trgState).mkString("\n").drop(5)
      s"\\begin{itemize} \\item Rule body: $bodyStr\n \\item Local profile: $localStr\n \\item Source profiles:\n\n \n$srcStrInItemize\n \\item Target profile:\n $trgStr \\end{itemize}"
    }


  }

  object entailmentCheckerResultToLatex {

    def apply(aut: EntailmentAutomaton, statesByPred: Map[String, Set[EntailmentProfile]]): String = {
      statesToLatex(statesByPred)
    }

    def statesToLatex(statesByPred: Map[String, Set[EntailmentProfile]]): String = {
      val lines = Stream("\\begin{itemize}") ++ statesByPred.toStream.flatMap(pair => Stream("\\item") ++ predToLatex(pair._1, pair._2)).map(indent) ++ Stream("\\end{itemize}")
      lines.mkString("\n")
    }

    def predToLatex(pred: String, states: Set[EntailmentProfile]): Stream[String] = {
      Stream(s"Reachable profiles for \\texttt{${Predicate.predicateHeadToLatex(pred)}}:", "\\begin{itemize}") ++ states.toStream.flatMap(s => stateToLatex(s)).map(indent) ++ Stream("\\end{itemize}")
    }

    def stateToLatex(state: EntailmentProfile): Stream[String] = {
      val header = s"\\item Profile with free variables ${state.params.toSeq.sorted.map(EntailmentResultToLatex.varToMath).mkString("$\\langle ", ", ", "\\rangle$")} and context decompositions:"

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
                                |\\tikzset{$NodeLabelStyleClass/.style={draw,rectangle,inner sep=2pt}}
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
