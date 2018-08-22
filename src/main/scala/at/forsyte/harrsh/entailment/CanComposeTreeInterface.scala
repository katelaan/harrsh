package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging

object CanComposeTreeInterface extends HarrshLogging {

  val canComposeTreeInterfaces: CanCompose[TreeInterface] = new CanCompose[TreeInterface] {
    override def avoidClashes(fst: TreeInterface, snd: TreeInterface): (TreeInterface, TreeInterface) = {
      val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.placeholderVarsInSubst)
      (fst.updateSubst(clashAvoidanceUpdate), snd)
    }

    override def root(a: TreeInterface): NodeLabel = a.root

    override def abstractLeaves(a: TreeInterface): Set[AbstractLeafNodeLabel] = a.leaves

    override def tryInstantiate(toInstantiate: TreeInterface, abstractLeaf: AbstractLeafNodeLabel, instantiation: TreeInterface, unification: Unification): Option[TreeInterface] = {
      assert(TreeInterface.haveNoConflicts(toInstantiate, instantiation),
        s"Overlapping placeholders between $toInstantiate and $instantiation")

      val propagateUnification = SubstitutionUpdate.fromUnification(unification)

      val newRoot = toInstantiate.root.update(propagateUnification)
      val allLeaves = (toInstantiate.leaves - abstractLeaf) ++ instantiation.leaves
      val newLeaves = allLeaves.map(_.update(propagateUnification))
      // FIXME: Doesn't this leak variables? I.e., don't we keep variables around that are only used at the merge point?
      val newUsageInfo = VarUsageByLabel.update(toInstantiate.usageInfo ++ instantiation.usageInfo, propagateUnification)
      // FIXME: In which cases should instantiation fail? Should we check for double allocation?
      val res = TreeInterface(newRoot, newLeaves, newUsageInfo)
      assert(TreeInterface.noGapsInPlaceholders(res),
        s"After instantiation, placeholder vars ${res.placeholderVarsInSubst} contain gap for tree interface $res")
      Some(res)
    }

    override def usageInfo(a: TreeInterface, n: NodeLabel): VarUsageInfo = {
      val res = n.subst.toSeq.map(a.usageInfo.getOrElse(_, VarUsage.Unused))
      logger.warn(s"Usage info for $n w.r.t. $a: $res")
      res
    }
  }

}
