package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.HeapAutomaton.Transition
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.inductive.{Predicate, Sid}
import at.forsyte.harrsh.util.ToLatex._

object EntailmentResultToLatex {

  private def indent(s : String) = "  " + s

  private def indent(ss : Stream[String]): Stream[String] = ss map indent

  def inTikzPic(lines: Stream[String], style: Option[String] = None): Stream[String] = {
    val styleString = style.map('[' + _ + ']').getOrElse("")
    Stream(s"\\begin{tikzpicture}$styleString") ++ lines ++ Stream("\\end{tikzpicture}")
  }

  private def positioning(prefix: String, dir: String, currId: Int) = {
    if (currId > 0) {
      val anchor = if (dir == "below") ".west, anchor=west" else ""
      val distance = if (dir == "below") "12" else "2"
      dir + "=" + distance + "mm of " + prefix + (currId-1) + anchor
    } else ""
  }

  object decompositionToLatexLines {

    def apply(decomp: ContextDecomposition, nodeId: String, position: String): Stream[String] = {
      val partsWithNodeIds = decomp.parts.toSeq.zipWithIndex map (pair => (pair._1, "c"+pair._2, positioning("c", "right", pair._2)))
      val partsTikz = partsWithNodeIds.toStream flatMap (pair => contextToLatexLines(pair._1, pair._2, pair._3, decomp.constraints.usage))
      val constraintsTikz = constraintsToLatexLines(decomp.constraints, positioning("c", "right", partsWithNodeIds.size))
      Stream(s"\\node[$DecompStyleClass,$position] ($nodeId) {") ++ indent(inTikzPic(partsTikz ++ constraintsTikz)) ++ Stream("};")
    }

    def contextToLatexLines(ctx: EntailmentContext, ctxId: String, position: String, usageInfo: VarUsageByLabel): Stream[String] = {
      val text = (ctx.root +: ctx.calls.toSeq).map(nodeLabelToLatex(_, usageInfo)).mkString(", ")
      Stream(s"\\node[$ContextStyleClass, $position] ($ctxId) {$text};")
    }

    def diseqToLatex(constraint: VarConstraints.DiseqConstraint): String = {
      val pair = constraint.toPair
      Var.indexedVarsToLatex(pair._1) + " \\neq " + Var.indexedVarsToLatex(pair._2)
    }

    def speculativeEqToLatex(eq: (Var,Var)): String = {
      Var.indexedVarToLatex(eq._1) + " = " + Var.indexedVarToLatex(eq._2)
    }

    private def constraintsToLatexLines(constraints: VarConstraints, position: String): Stream[String] = {
      val positions = Seq(position, positioning("constraints", "right", 1))
      val ensuredConstraints = (EnsuredStyleClass, constraints.ensuredDiseqs.map(diseqToLatex))
      val missingConstraints = (MissingStyleClass,
        constraints.speculativeDiseqs.map(diseqToLatex)
          ++ constraints.speculativeEqs.map(speculativeEqToLatex)
          ++ constraints.rewrittenSpeculation.map(_.toLatex))
      val constraintLists = Stream(ensuredConstraints, missingConstraints).filterNot(_._2.isEmpty)
      val constraintNodes = constraintLists.zipWithIndex.zip(positions).map{
        case (((style, list), index), pos) => (list.mkString("$", " \\wedge ", "$"), style, pos, index)
      }
      constraintNodes map {
        case (text, style, pos, id) =>
          s"\\node[$style, $pos] (constraints$id) {$text};"
      }
    }

    private def nodeLabelToLatex(nodeLabel: ContextPredCall, usageInfo: VarUsageByLabel): String = {
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
        case (from, to) => annotateWithUsageInfo(to)(Var.indexedVarsToLatex(to))
      }
      '$' + "\\mathtt{" + pred.headToLatex + "}" + paramLabels.mkString("(", ", ", ")") + '$'
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
      } yield predStr + ts.toSeq.sortBy(_.newInIteration).map(transitionToLatex).map("\\item Transition: " + _).mkString("\n\n") + endStr
      byPred.mkString("\n\n")
    }

    private def transitionToLatex(transition: Transition[EntailmentProfile]) : String = {
      val Transition(srcStates, body, Some(localState), headPredicate, trgState, iteration) = transition
      val localStr = entailmentCheckerResultToLatex.stateToLatex(localState).mkString("\n").drop(5)
      val srcStrs = (srcStates map (s => entailmentCheckerResultToLatex.stateToLatex(s).mkString("\n"))).mkString("\n\n")
      val srcStrInItemize = if (srcStrs.nonEmpty) s"\\begin{itemize}$srcStrs\\end{itemize}" else srcStrs
      val bodyStr = '$' + body.toLatex + '$'
      val trgStr = entailmentCheckerResultToLatex.stateToLatex(trgState).mkString("\n").drop(5)
      s"""\\begin{itemize}
         |  \\item Computed in iteration: $iteration
         |  \\item Rule body: $bodyStr
         |  \\item Local profile: $localStr
         |  \\item Source profiles:
         |
         |  $srcStrInItemize
         |  \\item Target profile: $trgStr
         |\\end{itemize}""".stripMargin
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
      val header = s"\\item Profile with free variables ${state.params.toSeq.sorted.map(Var.indexedVarToLatex).mkString("$\\langle ", ", ", "\\rangle$")}:"

      val decompsWithNodeNames = state.decompsOrEmptySet.toSeq.sortBy(_.parts.size).zipWithIndex.map(pair => (pair._1, "d" + pair._2, positioning("d", "below", pair._2)))
      val decompLines = decompsWithNodeNames.flatMap(pair => decompositionToLatexLines(pair._1, pair._2, pair._3))
      val nodesToFit = decompsWithNodeNames.map(_._2).map('(' + _ + ')').mkString("")
      val allLines = decompLines.toStream ++ Stream("\\begin{scope}[on background layer]", s"  \\node[profile,fit=$nodesToFit] {};", "\\end{scope}")
      Stream(header, "") ++ indent(inTikzPic(allLines))
    }

  }

  private val ProfileStyleClass = "profile"
  private val DecompStyleClass = "decomp"
  private val ContextStyleClass = "ctx"
  private val EnsuredStyleClass = "ensured"
  private val MissingStyleClass = "missing"
  private val SidPlaceholder = "SIDPLACEHOLDER"
  private val QueryPlaceholder = "QUERYPLACEHOLDER"
  private val ResultPlaceholder = "RESULTPLACEHOLDER"
  private val TransitionPlaceholder = "TRANSPLACEHOLDER"

  private val latexTemplate = s"""\\documentclass{article}
                                |\\usepackage[a2paper, landscape]{geometry}
                                |\\usepackage{amsmath,amssymb}
                                |\\usepackage{xcolor}
                                |\\DefineNamedColor{named}{azul2}{RGB}{40,96,127}
                                |
                                |\\newcommand{\\nil}{\\ensuremath{\\textnormal{\\textbf{null}}}}
                                |\\newcommand{\\emp}{\\ensuremath{\\textnormal{\\textbf{emp}}}}
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
                                |\\tikzset{$ProfileStyleClass/.style={fill=azul2!50}}
                                |\\tikzset{$DecompStyleClass/.style={fill=azul2!30}}
                                |\\tikzset{$ContextStyleClass/.style={fill=white}}
                                |\\tikzset{$EnsuredStyleClass/.style={fill=green!20}}
                                |\\tikzset{$MissingStyleClass/.style={fill=red!20}}
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
