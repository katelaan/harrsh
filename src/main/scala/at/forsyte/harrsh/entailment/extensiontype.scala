package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{FreeVar, Var}
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

  def nonPlaceholderFreeVars: Set[FreeVar] = {
    substs.flatMap(_.freeNonNullVars).filterNot(PlaceholderVar.isPlaceholder)
  }

  def placeholderVarsInSubst: Set[PlaceholderVar] = {
    substs.flatMap(_.placeholders)
  }

  def updateSubst(f: SubstitutionUpdate): TreeInterface = {
    TreeInterface(root.update(f), leaves map (_.update(f)))
  }

  private lazy val substs = leaves.map(_.subst) + root.subst
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

  def nonPlaceholderFreeVars: Set[FreeVar] = parts.flatMap(_.nonPlaceholderFreeVars)

  def placeholdersInSubst: Set[PlaceholderVar] = parts.flatMap(_.placeholderVarsInSubst)

  def dropVars(varsToDrop: Seq[Var]): ExtensionType = {
    // FIXME: Don't introduce redundant placeholder vars, i.e., if there is a variable alias that we don't drop, we don't need a new placeholder name to avoid empty sets in the substitution
    // Rename the vars to placeholder vars. To avoid clashes, shift the existing vars back.
    val placeholders = (1 to varsToDrop.size) map (PlaceholderVar(_))
    val pairs: Seq[(Var,Var)] = varsToDrop.zip(placeholders.map(_.toFreeVar))
    val replaceByPlacheolders: SubstitutionUpdate = SubstitutionUpdate.fromPairs(pairs)

    val shiftPlaceholders = PlaceholderVar.placeholderClashAvoidanceUpdate(placeholders.toSet)

    updateSubst(shiftPlaceholders).updateSubst(replaceByPlacheolders)
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
