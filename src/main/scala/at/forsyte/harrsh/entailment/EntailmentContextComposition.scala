package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{PureAtom, RichSid}

object EntailmentContextComposition extends HarrshLogging {

  def apply(sid: RichSid, fst: EntailmentContext, snd: EntailmentContext, usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker): Stream[(EntailmentContext, VarUsageByLabel, PureConstraintTracker)] = {
    for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(fst, snd)
      _ = logger.debug(s"Trying to compose on root ${t1.root} and leaf $n2")
      if !doubleAlloc(t1.root, n2, usageInfo)
      newEqualities = unificationEqualities(t1.root, n2)
      // FIXME: Do we have to support adding speculative equalities here? If so, we need to fix an apparent bug in the way they are currently propagated. And we might have to add them to local profile computation as well?
      if newEqualities.isEmpty
      pureWithNewEqualities = pureConstraints.addToMissing(newEqualities)
      propagateUnification = unification(t1.root, n2, usageInfo)
      instantiation = instantiate(t2, n2, t1, propagateUnification)
      _ = logger.debug("Will propagate unification into " + usageInfo)
      unifiedUsage = VarUsageByLabel.update(usageInfo, propagateUnification)
      //_ = assert(VarUsageByLabel.isWellFormed(unifiedUsage), "Overlapping entries in usage info: " + unifiedUsage)
      _ = logger.debug(s"Unified usage $usageInfo into $unifiedUsage")
      newPureConstraints = pureWithNewEqualities.update(propagateUnification)
    } yield (instantiation, unifiedUsage, newPureConstraints)
  }

  private def unificationEqualities(fst: ContextPredCall, snd: ContextPredCall): Seq[PureAtom] = {
    val res = for {
      (v1, v2) <- fst.subst.toSeq zip snd.subst.toSeq
      // If we're equating two vars which aren't already known to be equal...
      if v1 != v2 //v1.intersect(v2).isEmpty
      // ...and neither of these is a placeholder, then we have to explicitly make them equal
      if v1.exists(!PlaceholderVar.isPlaceholder(_)) && v2.exists(!PlaceholderVar.isPlaceholder(_))
    } yield PureAtom(v1.head, v2.head, isEquality = true)
    if (res.nonEmpty) {
      logger.debug(s"Unification of $fst and $snd imposes new equalities $res")
    }
    res
  }

  private def unification(fst: ContextPredCall, snd: ContextPredCall, usageInfo: VarUsageByLabel): SubstitutionUpdate = {
    val unification = (fst.subst.toSeq, snd.subst.toSeq).zipped.map(_ union _)
    logger.debug(s"Matching of $fst and $snd imposes unification $unification")
    SubstitutionUpdate.fromUnification(unification)
  }

  private case class CompositionInterface(ctxToEmbed: EntailmentContext, embeddingTarget: EntailmentContext, leafToReplaceInEmbedding: ContextPredCall)

  private def compositionCandidates(fst: EntailmentContext, snd: EntailmentContext): Stream[CompositionInterface] = {
    for {
      (ctxWithRoot, ctxWithAbstractLeaf) <- Stream((fst,snd), (snd,fst))
      root = ctxWithRoot.root
      abstractLeaf <- ctxWithAbstractLeaf.calls
      // Only consider for composition if the labeling predicates are the same
      if root.pred == abstractLeaf.pred
    } yield CompositionInterface(ctxWithRoot, ctxWithAbstractLeaf, abstractLeaf)
  }

  private def doubleAlloc(fst: ContextPredCall, snd: ContextPredCall, usage: VarUsageByLabel): Boolean = {
    assert(fst.pred == snd.pred)
    assert(fst.freeVarSeq == snd.freeVarSeq)
    (fst.subst.toSeq zip snd.subst.toSeq) exists {
      case (v1, v2) => {
        val res = usage(v1) == VarAllocated && usage(v2) == VarAllocated && v1.intersect(v2).isEmpty
        if (res) {
          logger.debug(s"Can't compose $fst and $snd: Cannot unify $v1 with $v2 because of double allocation wrt usage $usage.")
        }
        res
      }
    }
  }

  def instantiate(toInstantiate: EntailmentContext, abstractLeaf: ContextPredCall, instantiation: EntailmentContext, propagateUnification: SubstitutionUpdate): EntailmentContext = {
    assert(EntailmentContext.haveDisjointPlaceholders(toInstantiate, instantiation),
      s"Overlapping placeholders between $toInstantiate and $instantiation")

    val newRoot = toInstantiate.root.update(propagateUnification)
    val allLeaves = (toInstantiate.calls - abstractLeaf) ++ instantiation.calls
    val newLeaves = allLeaves.map(_.update(propagateUnification))
    EntailmentContext(newRoot, newLeaves)
  }

}
