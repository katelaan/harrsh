package at.forsyte.harrsh.entailment

import scala.annotation.tailrec

case class UnfoldingForest(trees: Seq[UnfoldingTree]) {

  def compose(other: UnfoldingForest): UnfoldingForest = {
    UnfoldingForest.merge(trees ++ other.trees)
  }

  def map[B](f: UnfoldingTree => B): Seq[B] = trees.map(f)

  def isConcrete: Boolean = trees.size == 1 && trees.head.isConcrete

  def toExtensionType: ExtensionType = ExtensionType(
    trees map {
      tree => TreeInterface(tree.nodeLabels(tree.root), tree.abstractLeaves.map(tree.nodeLabels).map(_.asInstanceOf[AbstractLeafNodeLabel]))
    }
  )

}

object UnfoldingForest {

  def merge(trees: Seq[UnfoldingTree]): UnfoldingForest = sweepingMerge(Seq.empty, trees)

  @tailrec private def sweepingMerge(processed: Seq[UnfoldingTree], unprocessed: Seq[UnfoldingTree]): UnfoldingForest = {
    if (unprocessed.isEmpty) {
      UnfoldingForest(processed)
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