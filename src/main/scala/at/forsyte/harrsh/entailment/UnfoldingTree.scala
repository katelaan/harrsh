package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.UnfoldingTree._
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._



sealed trait UTNode {
  val labels: Labeling

  def isAbstractLeaf: Boolean = this match {
    case _:RuleNode => false
    case _:AbstractLeafNode => true
  }
}

case class RuleNode(rule: Rule, override val labels: Labeling) extends UTNode {
  assert(labels.keySet == rule.body.freeVars.toSet)
}

case class AbstractLeafNode(pred: Predicate, override val labels: Labeling) extends UTNode {
  assert(labels.keySet == pred.defaultCall.freeVars.toSet)
}

case class UnfoldingTree(sid: SID, nodes: Set[UTNode], root: UTNode, children: Map[UTNode, Seq[UTNode]]) {

  val abstractLeaves: Set[AbstractLeafNode] = nodes.filter(_.isAbstractLeaf).map(_.asInstanceOf[AbstractLeafNode])

  assert(nodes.contains(root))
  assert(nodes forall (n => children(n).nonEmpty || n.isAbstractLeaf))
  assert(abstractLeaves forall (children(_).isEmpty))

  lazy val parents: Map[UTNode, UTNode] = {
    Map.empty ++ children.map(pair => pair._2.map(v => (pair._1, v))).flatten
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

}
