package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RichSid}

import scala.annotation.tailrec

object OldEntailmentContextComposition extends HarrshLogging {

  /**
    * Execute as many composition steps as possible on `as`, returning a result where no further composition steps are possible.
    */
  def composeAll(sid: RichSid, as: Seq[OldEntailmentContext]): Seq[OldEntailmentContext] = sweepingMerge(sid, Seq.empty, as)

  /**
    * Return all ways to compose zero or more of the elements of `as`.
    */
  def compositionOptions(sid: RichSid, as: Seq[OldEntailmentContext]): Seq[Seq[OldEntailmentContext]] = allMergeOptions(sid, Seq.empty, as)

  def compose(sid: RichSid, fst: OldEntailmentContext, snd: OldEntailmentContext): Option[OldEntailmentContext] = {
    logger.debug(s"Will try to compose $fst with $snd.")

    val shifted@(shiftedFst, shiftedSnd) = makeDisjoint(fst, snd)
    logger.debug(s"After shifting: $shifted")

    (for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(shiftedFst, shiftedSnd)
      unification <- tryUnify(sid, t1, t1.root, t2, n2)
      // Compose using the unification. (This can fail in case the unification leads to double allocation)
      instantiation <- tryInstantiate(t2, n2, t1, unification)
    } yield instantiation).headOption
  }

  def makeDisjoint(fst: OldEntailmentContext, snd: OldEntailmentContext): (OldEntailmentContext, OldEntailmentContext) = {
    val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.placeholders)
    (fst.updateSubst(clashAvoidanceUpdate, convertToNormalform = false), snd)
  }

  def tryInstantiate(toInstantiate: OldEntailmentContext, abstractLeaf: ContextPredCall, instantiation: OldEntailmentContext, unification: Unification): Option[OldEntailmentContext] = {
    assert(OldEntailmentContext.haveNoConflicts(toInstantiate, instantiation),
      s"Overlapping placeholders between $toInstantiate and $instantiation")

    val propagateUnification = SubstitutionUpdate.fromUnification(unification)

    val newRoot = toInstantiate.root.update(propagateUnification)
    val allLeaves = (toInstantiate.calls - abstractLeaf) ++ instantiation.calls
    val newLeaves = allLeaves.map(_.update(propagateUnification))

    val newUsageInfo = combineUsageInfo(toInstantiate.usageInfo, instantiation.usageInfo, propagateUnification, Set(newRoot) ++ newLeaves)
    val newDiseqs = (toInstantiate.pureConstraints ++ instantiation.pureConstraints).update(propagateUnification)

    val res = OldEntailmentContext(newRoot, newLeaves, newUsageInfo, newDiseqs, convertToNormalform = true)
    assert(OldEntailmentContext.isInNormalForm(res),
      s"After instantiation, placeholder vars ${res.placeholders} contain gap for context $res")

    // FIXME: In which cases should instantiation fail? Should we e.g. check for double allocation? (It seems like the answer is no: As long as the SID we analyze is ALL-SAT, there can never be double allocation when composing profiles.)
    Some(res)
  }

  private def combineUsageInfo(fst: VarUsageByLabel, snd: VarUsageByLabel, update: SubstitutionUpdate, labels: Iterable[ContextPredCall]): VarUsageByLabel = {
    val fstUpdated = VarUsageByLabel.update(fst, update)
    val sndUpdated = VarUsageByLabel.update(snd, update)
    val combinedUsageInfo = VarUsageByLabel.merge(fstUpdated, sndUpdated)
    VarUsageByLabel.restrictToSubstitutionsInLabels(combinedUsageInfo, labels)
  }

  private def rootsAreUsed(sid: RichSid, pred: Predicate, usage1: VarUsageInfo, usage2: VarUsageInfo): Boolean = {
    sid.rootParamIndex.get(pred.head) match {
      case Some(ix) =>
        val isUsed = usage1(ix).isUsed && usage2(ix).isUsed
        if (!isUsed) {
          logger.debug(s"Root parameter ${sid.roots(pred.head)} isn't marked as used in at least one of the calls, can't unify.")
        }
        isUsed
      case None =>
        throw new IllegalArgumentException("Can't perform unification for predicates without roots")
    }
  }

  private def tryUnify(sid: RichSid, a1: OldEntailmentContext, n1: ContextPredCall, a2: OldEntailmentContext, n2: ContextPredCall): Option[Unification] = {
    logger.debug(s"Will try to unify $n1 with $n2")
    assert(a1.root == n1)
    assert(a2.calls.contains(n2))
    assert(n1.freeVarSeq == n2.freeVarSeq)
    val (n1usage, n2usage) = (a1.usageInfoOfNode(n1), a2.usageInfoOfNode(n2))

    // TODO: If we want to relax the assumption about rootedness, the first condition has to be removed/relaxed. We'd have to ensure that this doesn't break soundness, though. See also the related TODO in LocalProfile
    if (rootsAreUsed(sid, n1.pred, n1usage, n2usage) && allUnifiable(sid, n1, n1usage, n2, n2usage)) {
      Some((n1.subst.toSeq, n2.subst.toSeq).zipped.map(_ union _))
    } else {
      None
    }
  }

  private def areUnifiable(sid: RichSid, pred: Predicate, v: FreeVar, subst1: Set[Var], usage1: VarUsage, subst2: Set[Var], usage2: VarUsage): Boolean = {
    (usage1, usage2) match {
      case (VarAllocated, VarAllocated) =>
        // Double allocation
        logger.debug(s"Can't unify FV $v because of double allocation.")
        false
      case (VarUnused, _) =>
        // Only used in one of the objects => Don't need to have a name in both
        true
      case (_, VarUnused) =>
        // Only used in one of the objects => Don't need to have a name in both
        true
      case (used1, used2) =>
        assert(used1.isUsed && used2.isUsed)
        // Used in both => Need a common name
        // TODO: Is it true that root parameters need to match, whereas for other parameters we can introduce aliasing?
        val res = (v != sid.roots(pred.head)) || (subst1 intersect subst2).nonEmpty
        if (!res) {
          logger.debug(s"Can't unify root params $v (Shared non-placeholder name required but not present.)")
        }
        res
    }
  }

  private def allUnifiable(sid: RichSid, n1: ContextPredCall, n1usage: VarUsageInfo, n2: ContextPredCall, n2usage: VarUsageInfo): Boolean = {
    (n1.freeVarSeq zip n1.subst.toSeq zip n1usage zip n2.subst.toSeq zip n2usage).forall {
      case ((((v, subst1), usage1), subst2), usage2) => areUnifiable(sid, n1.pred, v, subst1, usage1, subst2, usage2)
    }
  }

  case class CompositionInterface(ctxToEmbed: OldEntailmentContext, embeddingTarget: OldEntailmentContext, leafToReplaceInEmbedding: ContextPredCall)

  private def compositionCandidates(fst: OldEntailmentContext, snd: OldEntailmentContext): Stream[CompositionInterface] = {
    for {
      (ctxWithRoot, ctxWithAbstractLeaf) <- Stream((fst,snd), (snd,fst))
      root = ctxWithRoot.root
      abstractLeaf <- ctxWithAbstractLeaf.calls
      // Only consider for composition if the labeling predicates are the same
      if root.pred == abstractLeaf.pred
    } yield CompositionInterface(ctxWithRoot, ctxWithAbstractLeaf, abstractLeaf)
  }

  private def allMergeOptions(sid: RichSid, processed: Seq[OldEntailmentContext], unprocessed: Seq[OldEntailmentContext]): Seq[Seq[OldEntailmentContext]] = {
    if (unprocessed.isEmpty) {
      Seq(processed)
    } else {
      optionalMerge(sid, processed, unprocessed) flatMap {
        case (processedNew, unprocessedNew) => allMergeOptions(sid, processedNew, unprocessedNew)
      }
    }
  }

  private def optionalMerge(sid: RichSid, processed: Seq[OldEntailmentContext], unprocessed: Seq[OldEntailmentContext]): Seq[(Seq[OldEntailmentContext], Seq[OldEntailmentContext])] = {
    val (fst, other) = (unprocessed.head, unprocessed.tail)
    Seq((processed :+ fst, other)) ++ tryMerge(sid, fst, other).map(pair => (processed, pair._1 +: pair._2))
  }

  @tailrec private def sweepingMerge(sid: RichSid, processed: Seq[OldEntailmentContext], unprocessed: Seq[OldEntailmentContext]): Seq[OldEntailmentContext] = {
    if (unprocessed.isEmpty) {
      processed
    } else {
      tryMerge(sid, unprocessed.head, unprocessed.tail) match {
        case Some((merged, other)) => sweepingMerge(sid, processed, merged +: other)
        case None => sweepingMerge(sid, processed :+ unprocessed.head, unprocessed.tail)
      }
    }
  }

  private def tryMerge(sid: RichSid, fst: OldEntailmentContext, other: Seq[OldEntailmentContext]): Option[(OldEntailmentContext, Seq[OldEntailmentContext])] = {
    (for {
      candidate <- other.toStream
      composed <- compose(sid, fst, candidate)
    } yield (composed, other.filter(_ != candidate))).headOption
  }
  
}
