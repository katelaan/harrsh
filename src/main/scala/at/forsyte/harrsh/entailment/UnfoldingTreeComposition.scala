package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging

object UnfoldingTreeComposition {

  val canComposeUT: CanCompose[UnfoldingTree] = new CanCompose[UnfoldingTree] with HarrshLogging {
    override def avoidClashes(fst: UnfoldingTree, snd: UnfoldingTree): (UnfoldingTree, UnfoldingTree) = {
      val shifted = avoidIdClashes(fst, snd)
      val res = avoidPlaceHolderClashes(shifted._1, shifted._2)
      assert(UnfoldingTree.haveNoConflicts(res._1, res._2),
        s"After shifting/renaming, still conflicts between ${res._1} and ${res._2} (with placeholders ${res._1.placeholders} and ${res._2.placeholders} and nodes ${res._1.nodes} and ${res._2.nodes})")
      res
    }

    override def root(a: UnfoldingTree): NodeLabel = a.nodeLabels(a.root)

    override def abstractLeaves(a: UnfoldingTree): Set[AbstractLeafNodeLabel] = a.abstractLeaves map (a.nodeLabels(_).asInstanceOf[AbstractLeafNodeLabel])

    override def tryUnify(n1: NodeLabel, n2: NodeLabel): Option[Unification] = {
      logger.debug(s"Will try to unify $n1 with $n2")
      // FIXME: Proper unification
      assert(n1.freeVarSeq == n2.freeVarSeq)
      val fvars = n1.freeVarSeq
      if ((n1.rootVarSubst intersect n2.rootVarSubst).nonEmpty) {
        logger.debug(s"Can unify: Overlap between labels of root vars, ${n1.rootVarSubst} and ${n2.rootVarSubst}")
        Some((n1.subst.toSeq, n2.subst.toSeq).zipped.map(_ union _))
      } else {
        logger.debug("No unification possible")
        None
      }
    }

    override def tryInstantiate(toInstantiate: UnfoldingTree, abstractLeaf: NodeLabel, instantiation: UnfoldingTree, unification: Unification): Option[UnfoldingTree] = {
      val maybeNodeId = toInstantiate.abstractLeaves.find(
        id => toInstantiate.nodeLabels(id) == abstractLeaf
      )
      for {
        nodeId <- maybeNodeId
      } yield toInstantiate.instantiate(nodeId, instantiation, unification)
    }
  }

  private def avoidPlaceHolderClashes(fst: UnfoldingTree, snd: UnfoldingTree): (UnfoldingTree, UnfoldingTree) = {
    val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd)
    (fst.updateSubst(clashAvoidanceUpdate), snd)
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
    (UnfoldingTree(renamedLabels, renaming(fst.root), renamedChildren), snd)
  }

}
