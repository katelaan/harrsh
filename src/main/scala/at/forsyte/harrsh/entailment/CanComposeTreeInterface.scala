package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var

object CanComposeTreeInterface extends HarrshLogging {

  val canComposeTreeInterfaces: CanCompose[TreeInterface] = new CanCompose[TreeInterface] {
    override def makeDisjoint(fst: TreeInterface, snd: TreeInterface): (TreeInterface, TreeInterface) = {
      val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.placeholders)
      (fst.updateSubst(clashAvoidanceUpdate, convertToNormalform = false), snd)
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

      val combinedUsageInfo = combineUsageInfo(toInstantiate.usageInfo, instantiation.usageInfo, propagateUnification)
      val occurringSubstitutions: Set[Set[Var]] = (Set(newRoot) ++ newLeaves).flatMap(_.subst.toSeq)
      val newUsageInfo = combinedUsageInfo.filter(pair => occurringSubstitutions.contains(pair._1))
      val newDiseqs = (toInstantiate.diseqs compose instantiation.diseqs).update(propagateUnification)

      val res = TreeInterface(newRoot, newLeaves, newUsageInfo, newDiseqs, convertToNormalform = true)
      assert(TreeInterface.isInNormalForm(res),
        s"After instantiation, placeholder vars ${res.placeholders} contain gap for tree interface $res")

      // FIXME: In which cases should instantiation fail? Should we e.g. check for double allocation?
      Some(res)
    }

    private def combineUsageInfo(fst: VarUsageByLabel, snd: VarUsageByLabel, update: SubstitutionUpdate): VarUsageByLabel = {
      val fstUpdated = VarUsageByLabel.update(fst, update)
      val sndUpdated = VarUsageByLabel.update(snd, update)
      val keys = fstUpdated.keySet ++ sndUpdated.keySet
      val pairs: Set[(Set[Var], VarUsage)] = keys.map{
        k => (k, Seq(fstUpdated.getOrElse(k, VarUsage.Unused), sndUpdated.getOrElse(k, VarUsage.Unused)).max)
      }
      pairs.toMap
    }

    override def usageInfo(a: TreeInterface, n: NodeLabel): VarUsageInfo = {
      val res = n.subst.toSeq.map(a.usageInfo.getOrElse(_, VarUsage.Unused))
      logger.debug(s"Usage info for $n w.r.t. $a: $res")
      res
    }
  }

}
