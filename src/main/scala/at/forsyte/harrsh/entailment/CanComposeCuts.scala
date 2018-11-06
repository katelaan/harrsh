package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var

object CanComposeCuts extends HarrshLogging {

  val canComposeTreeInterfaces: CanCompose[TreeCut] = new CanCompose[TreeCut] {
    override def makeDisjoint(fst: TreeCut, snd: TreeCut): (TreeCut, TreeCut) = {
      val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.placeholders)
      (fst.updateSubst(clashAvoidanceUpdate, convertToNormalform = false), snd)
    }

    override def root(a: TreeCut): NodeLabel = a.root

    override def abstractLeaves(a: TreeCut): Set[PredicateNodeLabel] = a.leaves

    override def tryInstantiate(toInstantiate: TreeCut, abstractLeaf: PredicateNodeLabel, instantiation: TreeCut, unification: Unification): Option[TreeCut] = {
      assert(TreeCut.haveNoConflicts(toInstantiate, instantiation),
        s"Overlapping placeholders between $toInstantiate and $instantiation")

      val propagateUnification = SubstitutionUpdate.fromUnification(unification)

      val newRoot = toInstantiate.root.update(propagateUnification)
      val allLeaves = (toInstantiate.leaves - abstractLeaf) ++ instantiation.leaves
      val newLeaves = allLeaves.map(_.update(propagateUnification))

      val newUsageInfo = combineUsageInfo(toInstantiate.usageInfo, instantiation.usageInfo, propagateUnification, Set(newRoot) ++ newLeaves)
      val newDiseqs = (toInstantiate.pureConstraints compose instantiation.pureConstraints).update(propagateUnification)

      val res = TreeCut(newRoot, newLeaves, newUsageInfo, newDiseqs, convertToNormalform = true)
      assert(TreeCut.isInNormalForm(res),
        s"After instantiation, placeholder vars ${res.placeholders} contain gap for tree interface $res")

      // FIXME: In which cases should instantiation fail? Should we e.g. check for double allocation?
      Some(res)
    }

    private def combineUsageInfo(fst: VarUsageByLabel, snd: VarUsageByLabel, update: SubstitutionUpdate, labels: Iterable[PredicateNodeLabel]): VarUsageByLabel = {
      val fstUpdated = VarUsageByLabel.update(fst, update)
      val sndUpdated = VarUsageByLabel.update(snd, update)
      val combinedUsageInfo = VarUsageByLabel.merge(fstUpdated, sndUpdated)
      VarUsageByLabel.restrictToSubstitutionsInLabels(combinedUsageInfo, labels)
    }

    override def usageInfo(a: TreeCut, n: NodeLabel): VarUsageInfo = a.usageInfoOfNode(n)
  }

}
