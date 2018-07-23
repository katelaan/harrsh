package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.{Rule, SID, SymbolicHeap}

case class UTNode(lab: SymbolicHeap) {

}

case class UnfoldingTree(nodes: Set[UTNode], root: UTNode, abstractLeaves: Set[UTNode], children: Map[UTNode, Seq[UTNode]]) {

  assert(nodes.contains(root))
  assert(abstractLeaves subsetOf nodes)
  assert(nodes forall (n => children(n).nonEmpty || abstractLeaves.contains(n)))
  assert(abstractLeaves forall (children(_).isEmpty))

  lazy val parents: Map[UTNode, UTNode] = {
    Map.empty ++ children.map(pair => pair._2.map(v => (pair._1, v))).flatten
  }

//  lazy val children: Map[UTNode, Iterable[UTNode]] = {
//    parents.map(_.swap).groupBy(_._1).mapValues(_.values)
//  }

  def unfold(leaf: UTNode, rule: Rule): UnfoldingTree = {
    assert(nodes.contains(leaf))
    assert(abstractLeaves.contains(leaf))
    assert(children(leaf).isEmpty)
    assert(UnfoldingTree.isAbstract(leaf))
    val sh = leaf.lab
    assert(rule.head == sh.predCalls.head.name)
    val unfolded = sh.replaceCall(sh.predCalls.head, rule.body)
    val childLabels = unfolded.predCalls map (call => SymbolicHeap(Seq.empty, Seq(call)))
    val childNodes = childLabels map UTNode
    replace(leaf, UTNode(unfolded), childNodes)
  }

  def project(retainCalls: Boolean = false): SymbolicHeap = {

    def projectNode(node: UTNode): SymbolicHeap = {
      if (abstractLeaves.contains(node)) {
        if (retainCalls) node.lab else SymbolicHeap.empty
      } else {
        node.lab.replaceCalls(children(node).map(projectNode))
      }
    }

    projectNode(root)
  }

  private def replace(oldLeaf: UTNode, unfoldedLeaf: UTNode, newLeaves: Seq[UTNode]): UnfoldingTree = {
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

//    val maybeParentEdge = parents.get(oldLeaf)
//    val replacingEdge = maybeParentEdge.map((unfoldedLeaf,_))
//    val edgesFromNewLeaves = newLeaves.map((_,unfoldedLeaf))
//    val newParents = (parents -- maybeParentEdge) ++ replacingEdge ++ edgesFromNewLeaves
//    UnfoldingTree(newNodes, newRoot, newAbstractLeaves, newParents)

    UnfoldingTree(newNodes, newRoot, newAbstractLeaves, childrenWithNewLeaves)
  }

}

object UnfoldingTree {
  def fromPredicate(pred: String, sid: SID): UnfoldingTree = {
    val node = UTNode(sid.callToPred(pred))
    UnfoldingTree(Set(node), node, Set(node), Map.empty)
  }

  def isAbstract(node: UTNode): Boolean = {
    val sh = node.lab
    sh.pure.isEmpty && !sh.hasPointer && sh.predCalls.length == 1
  }

}
