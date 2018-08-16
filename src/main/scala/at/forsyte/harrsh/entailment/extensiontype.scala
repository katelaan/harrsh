package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive.PredCall

case class TreeInterface(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel]) {
  def isConcrete: Boolean = leaves.isEmpty

  def asExtensionType: ExtensionType = ExtensionType(Set(this))
}

/**
  * A single abstracted forest, retaining only the tree interfaces rather than the full trees.
  * @param parts Abstracted trees
  */
case class ExtensionType(parts: Set[TreeInterface]) {

  def isFinal(call: PredCall): Boolean = {
    if (parts.size != 1) {
      // It's not an abstracted tree, so it can't represent a single concrete unfolding tree
      false
    } else {
      val tif = parts.head
      val rootPred = tif.root.pred
      Stream(
        tif.isConcrete, // It represents a concrete tree...
        rootPred.head == call.name, // ...rooted in the correct predicate...
        (rootPred.params, call.args).zipped.forall{
          // ...and with the correct vector of variables at the root (corresponding to the goal predicate call)
          case (param, arg) => tif.root.subst(param).contains(arg.asInstanceOf[FreeVar])
        }
      ).forall(b => b)
    }
  }

}

object ExtensionType {

  def apply(parts: Seq[TreeInterface]): ExtensionType = ExtensionType(parts.toSet)

}
