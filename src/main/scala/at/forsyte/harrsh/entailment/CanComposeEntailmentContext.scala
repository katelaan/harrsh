package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var

object CanComposeEntailmentContext extends HarrshLogging {

  val canComposeTreeInterfaces: CanCompose[EntailmentContext] = new CanCompose[EntailmentContext] {
    override def makeDisjoint(fst: EntailmentContext, snd: EntailmentContext): (EntailmentContext, EntailmentContext) = {
      val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.placeholders)
      (fst.updateSubst(clashAvoidanceUpdate, convertToNormalform = false), snd)
    }

    override def root(a: EntailmentContext): NodeLabel = a.root

    override def abstractLeaves(a: EntailmentContext): Set[PredicateNodeLabel] = a.calls

    override def tryInstantiate(toInstantiate: EntailmentContext, abstractLeaf: PredicateNodeLabel, instantiation: EntailmentContext, unification: Unification): Option[EntailmentContext] = {
      assert(EntailmentContext.haveNoConflicts(toInstantiate, instantiation),
        s"Overlapping placeholders between $toInstantiate and $instantiation")

      val propagateUnification = SubstitutionUpdate.fromUnification(unification)

      val newRoot = toInstantiate.root.update(propagateUnification)
      val allLeaves = (toInstantiate.calls - abstractLeaf) ++ instantiation.calls
      val newLeaves = allLeaves.map(_.update(propagateUnification))

      val newUsageInfo = combineUsageInfo(toInstantiate.usageInfo, instantiation.usageInfo, propagateUnification, Set(newRoot) ++ newLeaves)
      val newDiseqs = (toInstantiate.pureConstraints compose instantiation.pureConstraints).update(propagateUnification)

      val res = EntailmentContext(newRoot, newLeaves, newUsageInfo, newDiseqs, convertToNormalform = true)
      assert(EntailmentContext.isInNormalForm(res),
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

    override def usageInfo(a: EntailmentContext, n: NodeLabel): VarUsageInfo = a.usageInfoOfNode(n)
  }

}
