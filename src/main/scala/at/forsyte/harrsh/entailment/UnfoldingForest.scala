package at.forsyte.harrsh.entailment

case class UnfoldingForest(trees: Set[UnfoldingTree]) {

  def compress: ExtensionType = ???

  def map[B](f: UnfoldingTree => B): Set[B] = trees map f

}

object UnfoldingForest {

  def apply(trees: UnfoldingTree*): UnfoldingForest = UnfoldingForest(trees.toSet)

}