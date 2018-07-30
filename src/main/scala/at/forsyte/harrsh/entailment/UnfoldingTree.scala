package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.UnfoldingTree._
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.util.ToLatex._
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.seplog.inductive.SymbolicHeap.ops._

// FIXME: Nodes need an ID to make it possible for the same node to occur multiple times in the same tree. (Granted, a strange corner case, but we can't avoid that, can we?)

sealed trait UTNode {

  val labels: Labeling

  def isAbstractLeaf: Boolean = this match {
    case _:RuleNode => false
    case _:AbstractLeafNode => true
  }

  def symbolicHeapLabel: String = this match {
    case RuleNode(rule, _) => '$' + rule.body.toLatex(rule.naming).replaceAllLiterally("α", """\alpha""") + '$'
    case AbstractLeafNode(pred, _) => '$' + pred.defaultCall.toLatex.replaceAllLiterally("α", """\alpha""") + '$'
  }

  def freeVarSeq: Seq[FreeVar] = this match {
    case RuleNode(rule, _) => rule.body.freeVars
    case AbstractLeafNode(pred, _) => pred.params
  }
}

case class RuleNode(rule: Rule, override val labels: Labeling) extends UTNode {
  assert(labels.keySet == rule.body.freeVars.toSet)
}

case class AbstractLeafNode(pred: Predicate, override val labels: Labeling) extends UTNode {
  assert(labels.keySet == pred.defaultCall.freeVars.toSet)
}

case class UnfoldingTree(sid: SID, nodes: Set[UTNode], root: UTNode, children: Map[UTNode, Seq[UTNode]]) {

  // FIXME: Validate that the tree is "sufficiently" labeled: It has free variables for all root parameters of the interface nodes + possibly for some other nodes as well (e.g., parameter corresponding to a backpointer in a dll)

  val abstractLeaves: Set[AbstractLeafNode] = nodes.filter(_.isAbstractLeaf).map(_.asInstanceOf[AbstractLeafNode])

  assert(nodes.contains(root))
  assert(nodes forall (n => children(n).nonEmpty || n.isAbstractLeaf || (!n.isAbstractLeaf && n.asInstanceOf[RuleNode].rule.isBaseRule)))
  assert(abstractLeaves forall (children(_).isEmpty))

  lazy val parents: Map[UTNode, UTNode] = {
    for {
      (parent, succs) <- children
      child <- succs
    } yield (child, parent)
  }

  def unfold(leaf: AbstractLeafNode, rule: Rule): UnfoldingTree = {
    assert(abstractLeaves.contains(leaf))
    assert(rule.head == leaf.pred.head)
    val unfolded = RuleNode(rule, leaf.labels)
    val childCalls = rule.body.predCalls
    val childLabels = childCalls map (propagateLabels(leaf.labels, _))
    val childNodes = (childCalls, childLabels).zipped.map {
      case (PredCall(head,_), labeling) => AbstractLeafNode(sid.preds(head), labeling)
    }
    replace(leaf, unfolded, childNodes)
  }

  def project(retainCalls: Boolean = false): SymbolicHeap = {

    def projectNode(node: UTNode): SymbolicHeap = {
      val withoutLabeling = node match {
        case RuleNode(rule, labels) =>
          val childProjections = children(node) map projectNode
          rule.body.replaceCalls(childProjections)
        case AbstractLeafNode(pred, labels) =>
          if (retainCalls) pred.defaultCall else SymbolicHeap.empty
      }
      withoutLabeling.copy(pure = withoutLabeling.pure ++ labelingToAtoms(node.labels))
    }

    projectNode(root)
  }

  private def replace(oldLeaf: AbstractLeafNode, unfoldedLeaf: RuleNode, newLeaves: Seq[AbstractLeafNode]): UnfoldingTree = {
    assert(unfoldedLeaf.rule.body.predCalls.size == newLeaves.size)

    val newNodes = ((nodes - oldLeaf) + unfoldedLeaf) ++ newLeaves
    val newRoot = if (root == oldLeaf) unfoldedLeaf else root
    val newAbstractLeaves = (abstractLeaves - oldLeaf) ++ newLeaves

    val maybeParent = parents.get(oldLeaf)
    val childrenWithUnfoldedLeaf = maybeParent match {
      case Some(parent) =>
        val updatedChildren = children(parent).map(child => if (child == oldLeaf) unfoldedLeaf else child)
        children.updated(parent, updatedChildren)
      case None =>
        children
    }
    val childrenWithNewLeaves = childrenWithUnfoldedLeaf.updated(unfoldedLeaf, newLeaves)

    UnfoldingTree(sid, newNodes, newRoot, childrenWithNewLeaves)
  }

  def extendLabeling(unification: Unification): UnfoldingTree = {
    // FIXME: Update the labeling based on the unification
    this
  }

  def instantiate(abstractLeaf: AbstractLeafNode, replacingTree: UnfoldingTree, unification: Unification): Option[UnfoldingTree] = {
    val thisExtended = this.extendLabeling(unification)
    val otherExtended = replacingTree.extendLabeling(unification)
    val combinedNodes = (thisExtended.nodes - abstractLeaf) ++ otherExtended.nodes
    // Connect the parent of the replaced leaf with the root of the replacing tree
    // FIXME: This should actually fail once we perform `extendLabeling` -> abstractLeaf won't be in the resulting tree!
    val parentOfReplacedLeaf = thisExtended.parents(abstractLeaf)
    val newParentsChildren = thisExtended.children(parentOfReplacedLeaf).map{
      child => if (child == abstractLeaf) replacingTree.root else child
    }
    val combinedChildren = thisExtended.children.updated(parentOfReplacedLeaf, newParentsChildren) ++ otherExtended.children
    Some(UnfoldingTree(sid, combinedNodes, thisExtended.root, combinedChildren)).filterNot(_.hasDoubleAllocation)
  }

  def compose(other: UnfoldingTree): Option[(UnfoldingTree, Unification)] = {
    (for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(this, other)
      unification <- tryUnify(t1.root, n2)
      // Compose using the unification. (This can fail in case the unification leads to double allocation)
      instantiation <- t2.instantiate(n2, t2, unification)
    } yield (instantiation, unification)).headOption
  }

  def hasDoubleAllocation: Boolean = {
    // FIXME: Implement double allocation check (and possibly other validation as well -- sufficiently many names in interface nodes!)
    false
  }

}

object UnfoldingTree {

  type Labeling = Map[FreeVar,Set[Var]]

  def labelingToAtoms(l: Labeling) : Iterable[PureAtom] = for {
    (k,vs) <- l
    v <- vs
  } yield k =:= v

  def propagateLabels(labels: Labeling, call: PredCall): Labeling = ???

  def fromPredicate(sid: SID, pred: String, labeling: Labeling): UnfoldingTree = {
    val node = AbstractLeafNode(sid.preds(pred), labeling)
    UnfoldingTree(sid, Set(node), node, Map.empty)
  }

  type Unification = Seq[Set[Var]]

  case class CompositionInterface(treeToEmbed: UnfoldingTree, embeddingTarget: UnfoldingTree, leafToReplaceInEmbedding: AbstractLeafNode)

  private def compositionCandidates(tree1: UnfoldingTree, tree2: UnfoldingTree): Stream[CompositionInterface] = {
    for {
      (treeWithRoot, treeWithAbstractLeaf) <- Stream((tree1,tree2), (tree2,tree1))
      root = treeWithRoot.root
      abstractLeaf <- treeWithAbstractLeaf.abstractLeaves
    } yield CompositionInterface(treeWithRoot, treeWithAbstractLeaf, abstractLeaf)
  }

  private def tryUnify(n1: UTNode, n2: UTNode): Option[Unification] = {
    // FIXME: Proper unification for rooted SIDs. Do we have to take nodes other than the root parameter into account? (Maybe not, because we can check that we have sufficiently many free variables in the individual UTs instead?!)
    assert(n1.freeVarSeq == n2.freeVarSeq)
    val fvars = n1.freeVarSeq
    if (n1.labels(fvars.head).intersect(n2.labels(fvars.head)).nonEmpty) {
      Some(fvars map (fv => n1.labels(fv) union n2.labels(fv)))
    } else {
      None
    }
  }

}
