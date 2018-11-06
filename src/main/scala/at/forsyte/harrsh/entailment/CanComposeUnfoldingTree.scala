package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging

object CanComposeUnfoldingTree {

  val canComposeUT: CanCompose[UnfoldingTree] = new CanCompose[UnfoldingTree] with HarrshLogging {
    override def makeDisjoint(fst: UnfoldingTree, snd: UnfoldingTree): (UnfoldingTree, UnfoldingTree) = {
      val shifted = avoidIdClashes(fst, snd)
      val res = avoidPlaceHolderClashes(shifted._1, shifted._2)
      assert(UnfoldingTree.haveNoConflicts(res._1, res._2),
        s"After shifting/renaming, still conflicts between ${res._1} and ${res._2} (with placeholders ${res._1.placeholders} and ${res._2.placeholders} and nodes ${res._1.nodes} and ${res._2.nodes})")
      res
    }

    override def root(a: UnfoldingTree): NodeLabel = a.nodeLabels(a.root)

    override def abstractLeaves(a: UnfoldingTree): Set[PredicateNodeLabel] = a.abstractLeaves map (a.nodeLabels(_).asInstanceOf[PredicateNodeLabel])

    override def tryInstantiate(toInstantiate: UnfoldingTree, abstractLeaf: PredicateNodeLabel, instantiation: UnfoldingTree, unification: Unification): Option[UnfoldingTree] = {
      assert(UnfoldingTree.haveNoConflicts(toInstantiate, instantiation))

      for {
        nodeId <- toInstantiate.abstractLeaves.find(
          id => toInstantiate.nodeLabels(id) == abstractLeaf
        )
      } yield instantiate(toInstantiate, nodeId, instantiation, unification)
    }

    private def instantiate(toInstantiate: UnfoldingTree, abstractLeaf: NodeId, replacingTree: UnfoldingTree, unification: Unification): UnfoldingTree = {
      logger.debug(s"Replacing $abstractLeaf in $toInstantiate with $replacingTree")
      val propagateUnification = SubstitutionUpdate.fromUnification(unification)
      val thisExtended = toInstantiate.updateSubst(propagateUnification, convertToNormalform = false)
      val otherExtended = replacingTree.updateSubst(propagateUnification, convertToNormalform = false)
      // TODO: This instantiation 'leaks' the ID of abstractLeaf: It will not be used in the tree that we get after instantiation. I'm afraid this may complicate debugging.
      val combinedNodeLabels = (thisExtended.nodeLabels ++ otherExtended.nodeLabels) - abstractLeaf

      val (updatedRoot, updatedChildren) = if (abstractLeaf != toInstantiate.root) {
        (thisExtended.root, replaceAbstractLeafInChildren(thisExtended.children, thisExtended.parents, abstractLeaf, otherExtended.root))
      } else {
        (otherExtended.root, thisExtended.children)
      }
      // TODO: Another position where the 'leak' manifests
      val combinedChildren = (updatedChildren ++ otherExtended.children) - abstractLeaf

      UnfoldingTree(combinedNodeLabels, updatedRoot, combinedChildren, convertToNormalform = true)
    }

    private def replaceAbstractLeafInChildren(children: Map[NodeId, Seq[NodeId]], parents: Map[NodeId, NodeId], abstractLeaf: NodeId, root: NodeId) = {
      // Connect the parent of the replaced leaf with the root of the replacing tree
      val maybeParent = parents.get(abstractLeaf)
      maybeParent match {
        case Some(parent) =>
          val newParentsChildren = children(parent).map{
            child => if (child == abstractLeaf) root else child
          }
          logger.debug(s"Updating children of $parent from ${children(parent)} to $newParentsChildren")
          children.updated(parent, newParentsChildren)
        case None =>
          children
      }
    }


    override def usageInfo(a: UnfoldingTree, n: NodeLabel): VarUsageInfo = {
      n.freeVarSeq.map(a.findUsage(n,_))
    }
  }

  private def avoidPlaceHolderClashes(fst: UnfoldingTree, snd: UnfoldingTree): (UnfoldingTree, UnfoldingTree) = {
    val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd)
    (fst.updateSubst(clashAvoidanceUpdate, convertToNormalform = false), snd)
  }

  private def avoidIdClashes(fst: UnfoldingTree, snd: UnfoldingTree): (UnfoldingTree, UnfoldingTree) = {
    val nodes = snd.nodes
    val fresh = NodeId.freshIds(usedIds = nodes, numIds = fst.nodes.size)
    val renaming: Map[NodeId, NodeId] = (fst.nodes.toSeq.sorted, fresh).zipped.toMap
    val renamedLabels = fst.nodeLabels map {
      case (k, l) => (renaming(k), l)
    }
    val renamedChildren = fst.children map {
      case (p, cs) => (renaming(p), cs map renaming)
    }
    (UnfoldingTree(renamedLabels, renaming(fst.root), renamedChildren, convertToNormalform = false), snd)
  }

}
