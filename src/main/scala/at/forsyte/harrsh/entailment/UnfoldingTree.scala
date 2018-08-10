package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

case class UnfoldingTree(sid: SID, nodeLabels: Map[NodeId,NodeLabel], root: NodeId, children: Map[NodeId, Seq[NodeId]]) extends HarrshLogging {

  // FIXME: Validate that the tree is "sufficiently" labeled: It has free variables for all root parameters of the interface nodes + possibly for some other nodes as well (e.g., parameter corresponding to a backpointer in a dll)

  import at.forsyte.harrsh.entailment.UnfoldingTree._

  lazy val nodes: Set[NodeId] = nodeLabels.keySet

  lazy val abstractLeaves: Set[NodeId] = nodes.filter(n => nodeLabels(n).isAbstractLeaf)

  def isAbstractLeaf(nodeId: NodeId): Boolean = nodeLabels(nodeId).isAbstractLeaf

  assert(nodes.contains(root))
  assert(children.keys forall nodes.contains)
  assert(children.values forall (_ forall nodes.contains))
  assert(abstractLeaves forall (children(_).isEmpty))
  assert(nodes forall (n => children(n).nonEmpty
    || isAbstractLeaf(n)
    || (!isAbstractLeaf(n) && nodeLabels(n).asInstanceOf[RuleNodeLabel].rule.isBaseRule)))

  lazy val parents: Map[NodeId, NodeId] = {
    for {
      (parent, succs) <- children
      child <- succs
    } yield (child, parent)
  }

  override def toString: String = {
    val sb = new StringBuilder("UnfoldingTree(\n")
    for {
      id <- nodes.toSeq.sorted
    } {
      sb.appendAll(s"  $id -> ${nodeLabels(id)} : ${children(id).mkString(", ")}\n")
    }
    sb.appendAll(")\n")
    sb.toString
  }

  def unfold(leaf: NodeId, rule: Rule): UnfoldingTree = {
    assert(abstractLeaves.contains(leaf))
    assert(rule.head == nodeLabels(leaf).asInstanceOf[AbstractLeafNodeLabel].pred.head)
    val subst = nodeLabels(leaf).subst
    val unfolded = RuleNodeLabel(rule, subst)
    val childCalls = rule.body.predCalls
    val childIds = NodeId.freshIds(usedIds = nodes, numIds = childCalls.length)
    // TODO: Introduce unique placeholder vars in propagation...
    val childSubst = childCalls map subst.propagate
    val childNodes = (childIds, childCalls, childSubst).zipped.map {
      case (nodeId, PredCall(head,_), childLabeling) => (nodeId, AbstractLeafNodeLabel(sid.preds(head), childLabeling))
    }

    val newLabels = nodeLabels.updated(leaf, unfolded) ++ childNodes
    val newChildren = children + (leaf -> childIds)
    UnfoldingTree(sid, newLabels, root, newChildren)
  }

  def project(retainCalls: Boolean = false): SymbolicHeap = {

    def projectNode(node: NodeId): SymbolicHeap = {
      val label = nodeLabels(node)
      val withoutSubst = label match {
        case RuleNodeLabel(rule, _) =>
          val childProjections = children(node) map projectNode
          rule.body.replaceCalls(childProjections)
        case AbstractLeafNodeLabel(pred, _) =>
          if (retainCalls) pred.defaultCall else SymbolicHeap.empty
      }
      withoutSubst.copy(pure = withoutSubst.pure ++ label.subst.toAtoms)
    }

    projectNode(root)
  }

  private def avoidClashesWith(other: UnfoldingTree): UnfoldingTree = {
    val shifted = avoidIdClashWith(other)
    val res = shifted.avoidPlaceHolderClashWith(other)
    assert(haveNoConflicts(this, res))
    res
  }

  private def avoidPlaceHolderClashWith(other: UnfoldingTree): UnfoldingTree = {
    val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(other)
    updateSubst(clashAvoidanceUpdate)
  }

  private def avoidIdClashWith(other: UnfoldingTree): UnfoldingTree = {
    val nodes = other.nodes
    val fresh = NodeId.freshIds(usedIds = nodes, numIds = this.nodes.size)
    val renaming: Map[NodeId, NodeId] = (this.nodes.toSeq.sorted, fresh).zipped.toMap
    val renamedLabels = nodeLabels map {
      case (k, l) => (renaming(k), l)
    }
    val renamedChildren = children map {
      case (p, cs) => (renaming(p), cs map renaming)
    }
    UnfoldingTree(sid, renamedLabels, renaming(root), renamedChildren)
  }

  private def dropRedundantPlaceholders: UnfoldingTree = {
    def getRedundantVars(fvs: Set[FreeVar]): Set[FreeVar] = {
      val (phs, nonPhs) = Combinators.splitBy(fvs, PlaceholderVar.isPlaceholder)
      if (nonPhs.nonEmpty) {
        // There is a proper free var in this equivalence class => discard all equivalent placeholders
        phs
      } else {
        // Keep only the smalles placeholder among multiple placeholders
        val typedPhs = phs map (ph => PlaceholderVar.fromVar(ph).get)
        phs - PlaceholderVar.min(typedPhs).toFreeVar
      }
    }
    val equivalenceClasses = varEquivClasses(nodeLabels.values)
    val redundantVars = equivalenceClasses.flatMap(getRedundantVars)
    logger.debug(s"Reundant vars: $redundantVars")

    val updateF: FreeVar => Set[FreeVar] = {
      v => if (redundantVars.contains(v)) Set.empty else Set(v)
    }
    updateSubst(updateF)
  }

  def extendLabeling(unification: Unification): UnfoldingTree = {
    val updateFn : FreeVar => Set[FreeVar] = {
      v => unification.find(_.contains(v)).getOrElse(Set(v))
    }
    updateSubst(updateFn)
  }

  def instantiate(abstractLeaf: NodeId, replacingTree: UnfoldingTree, unification: Unification): Option[UnfoldingTree] = {
    assert(haveNoConflicts(this, replacingTree))
    logger.debug(s"Replacing $abstractLeaf in $this with $replacingTree")
    val thisExtended = this.extendLabeling(unification)
    val otherExtended = replacingTree.extendLabeling(unification)
    // TODO: This instantiation 'leaks' the ID of abstractLeaf: It will not be used in the tree that we get after instantiation. I'm afraid this may complicate debugging.
    val combinedNodeLabels = (thisExtended.nodeLabels ++ otherExtended.nodeLabels) - abstractLeaf

    // Connect the parent of the replaced leaf with the root of the replacing tree
    val parentOfReplacedLeaf = thisExtended.parents(abstractLeaf)
    val newParentsChildren = thisExtended.children(parentOfReplacedLeaf).map{
      child => if (child == abstractLeaf) otherExtended.root else child
    }
    logger.debug(s"Updating children of $parentOfReplacedLeaf from ${thisExtended.children(parentOfReplacedLeaf)} to $newParentsChildren")
    // TODO: Another position where the 'leak' manifests
    val combinedChildren = (thisExtended.children.updated(parentOfReplacedLeaf, newParentsChildren) ++ otherExtended.children) - abstractLeaf

    val combinedTree = UnfoldingTree(sid, combinedNodeLabels, thisExtended.root, combinedChildren)
    val reducedTree = combinedTree.dropRedundantPlaceholders

    // Discard trees with double allocation
    Some(reducedTree).filterNot(_.hasDoubleAllocation)
  }

  def compose(other: UnfoldingTree): Option[(UnfoldingTree, Unification)] = {
    logger.debug(s"Will try to compose $this with $other.")
    val shifted = other.avoidClashesWith(this)
    logger.debug(s"Other after shifting to avoid clashes with $nodes and $placeholders: $shifted")

    (for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(this, shifted)
      unification <- tryUnify(t1.nodeLabels(t1.root), t2.nodeLabels(n2))
      // Compose using the unification. (This can fail in case the unification leads to double allocation)
      instantiation <- t2.instantiate(n2, t1, unification)
    } yield (instantiation, unification)).headOption
  }

  def hasDoubleAllocation: Boolean = {
    // FIXME: Implement double allocation check (and possibly other validation as well -- sufficiently many names in interface nodes!)
    false
  }

  def placeholders: Set[PlaceholderVar] = nodeLabels.values.flatMap(_.placeholders).toSet

  def updateSubst(f: FreeVar => Set[FreeVar]): UnfoldingTree = {
    val updatedLabels = nodeLabels.map {
      case (id,label) => (id, label.update(f))
    }
    copy(nodeLabels = updatedLabels)
  }

}

object UnfoldingTree extends HarrshLogging {

  def varEquivClasses(labels: Iterable[NodeLabel]) : Set[Set[FreeVar]] = {
    // TODO: More efficient solution
    var candidates: Set[Set[FreeVar]] = for {
      label: NodeLabel <- labels.toSet
      vs <- label.subst.toMap.values
    } yield vs
    mergeOverlapping(candidates)
  }

  @tailrec private def mergeOverlapping(candidates: Set[Set[FreeVar]]): Set[Set[FreeVar]] = getOverlapping(candidates) match {
    case Some((set1, set2)) =>
      val merged = candidates - set1 - set2 + (set1 union set2)
      mergeOverlapping(merged)
    case None => candidates
  }

  private def getOverlapping(sets: Set[Set[FreeVar]]): Option[(Set[FreeVar], Set[FreeVar])] = {
    (for {
      set1 <- sets
      set2 <- sets
      if set1 != set2
      if (set1 intersect set2).nonEmpty
    } yield (set1, set2)).headOption
  }

  def haveNoConflicts(ut1: UnfoldingTree, ut2: UnfoldingTree) : Boolean = {
    (ut1.nodes intersect ut2.nodes).isEmpty && (ut1.placeholders intersect ut2.placeholders).isEmpty
  }

  def fromPredicate(sid: SID, pred: String, labeling: Substitution): UnfoldingTree = {
    val node = AbstractLeafNodeLabel(sid.preds(pred), labeling)
    UnfoldingTree(sid, Map(NodeId.zero -> node), NodeId.zero, Map.empty)
  }

  case class CompositionInterface(treeToEmbed: UnfoldingTree, embeddingTarget: UnfoldingTree, leafToReplaceInEmbedding: NodeId)

  private def compositionCandidates(tree1: UnfoldingTree, tree2: UnfoldingTree): Stream[CompositionInterface] = {
    for {
      (treeWithRoot, treeWithAbstractLeaf) <- Stream((tree1,tree2), (tree2,tree1))
      root = treeWithRoot.root
      abstractLeaf <- treeWithAbstractLeaf.abstractLeaves
    } yield CompositionInterface(treeWithRoot, treeWithAbstractLeaf, abstractLeaf)
  }

  private def tryUnify(n1: NodeLabel, n2: NodeLabel): Option[Unification] = {
    logger.debug(s"Will try to unify $n1 with $n2")
    // FIXME: Proper unification for rooted SIDs. Do we have to take nodes other than the root parameter into account? (Maybe not, because we can check that we have sufficiently many free variables in the individual UTs instead?!)
    assert(n1.freeVarSeq == n2.freeVarSeq)
    val fvars = n1.freeVarSeq
    if (n1.subst(fvars.head).intersect(n2.subst(fvars.head)).nonEmpty) {
      logger.debug(s"Can unify: Overlap between ${n1.subst(fvars.head)} and ${n2.subst(fvars.head)}")
      Some(fvars map (fv => n1.subst(fv) union n2.subst(fv)))
    } else {
      logger.debug("No unification possible")
      None
    }
  }

}
