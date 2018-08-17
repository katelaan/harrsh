package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

object ForestsToLatex {

  implicit val treeToLatex : ToLatex[UnfoldingTree] = (ut:UnfoldingTree, _:Naming) => {
    val lines = treeToLatexLines(ut, ut.root, TikzConfig(width = 24))
    wrapInTikzPic(lines)
  }

  implicit val forestToLatex : ToLatex[UnfoldingForest] = (uf: UnfoldingForest, _:Naming) => {
    val treeTex = uf map (_.toLatex)
    val treeNodes = treeTex.zipWithIndex map {
      case (tree, id) =>
        val pos = if (id == 0) "" else s"right=of t${id-1}"
        s"\\node[$pos] (t${id}) {\n$tree\n};"
    }
    wrapInTikzPic(treeNodes, style = Some("uf"))
  }

  private def wrapInTikzPic(lines: Iterable[String], style: Option[String] = None): String = {
    val styleString = style.map('[' + _ + ']').getOrElse("")
    lines.mkString(
      s"\\begin{tikzpicture}$styleString\n",
      "\n",
      "\n\\end{tikzpicture}"
    )
  }

  case class TikzConfig(width: Double, step: Double = -1.5, style: Option[String] = Some("utnode"), interfaceStyle: Option[String] = None)

  private def mkStyle(ut: UnfoldingTree, nodeId: NodeId, config: TikzConfig): String = {
    val isRoot = ut.root == nodeId
    val isInterface = isRoot || ut.isAbstractLeaf(nodeId)
    val baseStyle = config.style.getOrElse("")

    (isInterface, config.interfaceStyle) match {
      case (false, _) => baseStyle
      case (true, None) => baseStyle
      case (true, Some(style)) => if (baseStyle != "") baseStyle + "," + style else baseStyle
    }
  }

  private def treeToLatexLines(ut: UnfoldingTree, startFrom: NodeId, config: TikzConfig, level: Int = 0, index: Int = 0): Seq[String] = {

    val numUnits = Math.pow(2, level+1)
    val rootUnits = 1 + 2*index
    val xPos = config.width * rootUnits / numUnits
    val yPos = level * config.step
    val name = "id" + startFrom

    val nodeLabel = ut.nodeLabels(startFrom)
    val tikzNodeLabel = nodeLabel.symbolicHeapLabel
    val style = mkStyle(ut, startFrom, config)
    val substLatex = labelingToNodePart(nodeLabel)
    val lines = Seq(s"\\node[$style] at ($xPos,$yPos) ($name) {$tikzNodeLabel \\nodepart{two} \\footnotesize $substLatex};")

    val succs = ut.children(startFrom)
    val leftTree: Seq[String] = succs.headOption match {
      case None => Seq.empty
      case Some(left) =>
        val leftName = "id" + left.hashCode
        (treeToLatexLines(ut, left, config, level + 1, 2 * index)
          ++ Seq(s"\\draw ({$name}) edge[->] ({$leftName});"))
    }
    val rightTree: Seq[String] = succs.drop(1).headOption match {
      case None => Seq.empty
      case Some(right) =>
        val rightName = "id" + right.hashCode
        (treeToLatexLines(ut, right, config, level + 1, 2 * index + 1)
          ++ Seq(s"\\draw ({$name}) edge[->] ({$rightName});"))
    }

    lines ++ leftTree ++ rightTree
  }

  private val varsToMath = Naming.indexify(Naming.DefaultNaming)

  private def labelingToNodePart(nodeLabel: NodeLabel) = {
    val (pred, subst) = (nodeLabel.pred, nodeLabel.subst)
    val pairs = (pred.params, subst.toSeq).zipped.map {
      case (from, to) => varsToMath(from) + " \\rightarrow " + to.map(varsToMath).mkString(",")
    }
    '$' + pairs.mkString(";") + '$'
  }

}
