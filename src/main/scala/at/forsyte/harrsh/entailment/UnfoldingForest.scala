package at.forsyte.harrsh.entailment

import scala.annotation.tailrec

case class UnfoldingForest(trees: Set[UnfoldingTree]) {

  def compose(other: UnfoldingForest): UnfoldingForest = {
    UnfoldingForest.merge(trees.toSeq ++ other.trees)
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

  def merge(trees: Seq[UnfoldingTree]): UnfoldingForest = sweepingMerge(Seq.empty, trees)

  @tailrec private def sweepingMerge(processed: Seq[UnfoldingTree], unprocessed: Seq[UnfoldingTree]): UnfoldingForest = {
    if (unprocessed.isEmpty) {
      UnfoldingForest(processed.toSet)
    } else {
      tryMerge(unprocessed.head, unprocessed.tail) match {
        case Some((merged, other)) => sweepingMerge(processed, merged +: other)
        case None => sweepingMerge(processed :+ unprocessed.head, unprocessed.tail)
      }
    }
  }

  private def tryMerge(tree: UnfoldingTree, other: Seq[UnfoldingTree]): Option[(UnfoldingTree, Seq[UnfoldingTree])] = {
    (for {
      candidate <- other.toStream
      composed <- tree.compose(candidate)
    } yield (composed._1, other.filter(_ != candidate))).headOption
  }

}