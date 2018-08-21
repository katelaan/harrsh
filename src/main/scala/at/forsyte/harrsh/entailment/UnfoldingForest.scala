package at.forsyte.harrsh.entailment

case class UnfoldingForest(trees: Set[UnfoldingTree]) {

  def compose(other: UnfoldingForest): UnfoldingForest = {
    UnfoldingForest(CanCompose.composeAll(trees.toSeq ++ other.trees).toSet)
  }

  def ordered: Seq[UnfoldingTree] = trees.toSeq.sortBy(tree => {
    val rootLabel = tree.nodeLabels(tree.root)
    rootLabel.rootVarSubst.min
  })

  def map[B](f: UnfoldingTree => B): Set[B] = trees.map(f)

  def isConcrete: Boolean = trees.size == 1 && trees.head.isConcrete

  def toExtensionType: ExtensionType = ExtensionType(trees map (_.interface))

}

object UnfoldingForest {

  implicit val forestToLatex = ForestsToLatex.forestToLatex

}