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

    override def abstractLeaves(a: UnfoldingTree): Set[AbstractLeafNodeLabel] = a.abstractLeaves map (a.nodeLabels(_).asInstanceOf[AbstractLeafNodeLabel])

    override def tryInstantiate(toInstantiate: UnfoldingTree, abstractLeaf: AbstractLeafNodeLabel, instantiation: UnfoldingTree, unification: Unification): Option[UnfoldingTree] = {
      assert(UnfoldingTree.haveNoConflicts(toInstantiate, instantiation))

      val maybeNodeId = toInstantiate.abstractLeaves.find(
        id => toInstantiate.nodeLabels(id) == abstractLeaf
      )
      for {
        nodeId <- maybeNodeId
      } yield toInstantiate.instantiate(nodeId, instantiation, unification)
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
