package at.forsyte.harrsh.entailment

case class UnfoldingForest(trees: Set[UnfoldingTree]) {

  assert(trees forall UnfoldingTree.isInNormalForm,
  s"Trying to construct unfolding forest from non-normalized trees ${trees.mkString(", ")}")

  def compose(other: UnfoldingForest): UnfoldingForest = {
    UnfoldingForest(CanCompose.composeAll(trees.toSeq ++ other.trees).toSet)
  }

  def ordered: Seq[UnfoldingTree] = trees.toSeq.sortBy(tree => {
    val rootLabel = tree.nodeLabels(tree.root)
    rootLabel.rootVarSubst.min
  })

  def map[B](f: UnfoldingTree => B): Set[B] = trees.map(f)

  def isConcrete: Boolean = trees.size == 1 && trees.head.isConcrete

  def toExtensionTypeWithoutDisequalities: ExtensionType = ExtensionType(trees map (_.interface(DisequalityTracker.empty)))

}

object UnfoldingForest {

  implicit val forestToLatex = ForestsToLatex.forestToLatex

}