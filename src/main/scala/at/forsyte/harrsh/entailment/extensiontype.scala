package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive.PredCall

case class TreeInterface(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel]) {
  def isConcrete: Boolean = leaves.isEmpty

  def asExtensionType: ExtensionType = ExtensionType(Set(this))

  def asDegenerateTree: UnfoldingTree = {
    val ids = NodeId.freshIds(Set.empty, leaves.size + 1)
    val rootId = ids.head
    val nodeLabels = Map(rootId -> root) ++ (ids.tail, leaves).zipped
    val children = Map(rootId -> ids.tail) ++ ids.tail.zip(Stream.continually(Seq.empty))
    UnfoldingTree(nodeLabels, rootId, children)
  }

  def updateSubst(f: SubstitutionUpdate): TreeInterface = {
    TreeInterface(root.update(f), leaves map (_.update(f)))
  }
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
        (call.args, tif.root.subst.toSeq).zipped.forall{
          // ...and with the correct vector of variables at the root (corresponding to the goal predicate call)
          case (arg, substVal) => substVal.contains(arg)
        }
      ).forall(b => b)
    }
  }

  def updateSubst(f: SubstitutionUpdate): ExtensionType = ExtensionType(parts map (_.updateSubst(f)))

  def asDegenerateForest = UnfoldingForest(parts map (_.asDegenerateTree))

  def compose(other: ExtensionType): ExtensionType = {
    val thisForest = asDegenerateForest
    val otherForest = other.asDegenerateForest
    (thisForest compose otherForest).toExtensionType
  }

}

object ExtensionType {

  def apply(parts: Seq[TreeInterface]): ExtensionType = ExtensionType(parts.toSet)

}
