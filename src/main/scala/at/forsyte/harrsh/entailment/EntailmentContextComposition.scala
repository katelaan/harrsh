package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.FreeVar

import scala.annotation.tailrec

object EntailmentContextComposition extends HarrshLogging {

  /**
    * Execute as many composition steps as possible on `as`, returning a result where no further composition steps are possible.
    */
  def composeAll(as: Seq[EntailmentContext]): Seq[EntailmentContext] = sweepingMerge(Seq.empty, as)

  /**
    * Return all ways to compose zero or more of the elements of `as`.
    */
  def compositionOptions(as: Seq[EntailmentContext]): Seq[Seq[EntailmentContext]] = allMergeOptions(Seq.empty, as)

  def compose(fst: EntailmentContext, snd: EntailmentContext): Option[EntailmentContext] = {
    logger.debug(s"Will try to compose $fst with $snd.")

    val shifted@(shiftedFst, shiftedSnd) = makeDisjoint(fst, snd)
    logger.debug(s"After shifting: $shifted")

    (for {
      CompositionInterface(t1, t2, n2) <- compositionCandidates(shiftedFst, shiftedSnd)
      unification <- tryUnify(t1, t1.root, t2, n2)
      // Compose using the unification. (This can fail in case the unification leads to double allocation)
      instantiation <- tryInstantiate(t2, n2, t1, unification)
    } yield instantiation).headOption
  }

  def makeDisjoint(fst: EntailmentContext, snd: EntailmentContext): (EntailmentContext, EntailmentContext) = {
    val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.placeholders)
    (fst.updateSubst(clashAvoidanceUpdate, convertToNormalform = false), snd)
  }

  def tryInstantiate(toInstantiate: EntailmentContext, abstractLeaf: PredicateNodeLabel, instantiation: EntailmentContext, unification: Unification): Option[EntailmentContext] = {
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

  private def tryUnify(a1: EntailmentContext, n1: NodeLabel, a2: EntailmentContext, n2: PredicateNodeLabel): Option[Unification] = {
    logger.debug(s"Will try to unify $n1 with $n2")
    assert(a1.root == n1)
    assert(a2.calls.contains(n2))
    assert(n1.freeVarSeq == n2.freeVarSeq)
    val (n1usage, n2usage) = (a1.usageInfoOfNode(n1), a2.usageInfoOfNode(n2))

    // Sanity check: The root parameter of the predicate is marked as used in both nodes
    // TODO: If we want to relax the assumption about rootedness, this has to go. We'd have to ensure that this doesn't break soundness, though. See also the related TODO in LocalProfile
    n1.pred.rootParam foreach {
      param =>
        val ix = n1.freeVarSeq.indexOf(param)
        assert(n1usage(ix).isUsed && n2usage(ix).isUsed,
          s"Root parameter ${n1.pred.rootParam.get} isn't marked as used in at least one of $n1 and $n2 (usage info: ${(n1.subst.toSeq zip n1usage).mkString(",")}; ${(n2.subst.toSeq zip n2usage).mkString(",")})")
    }

    val unifiableParams: Seq[Boolean] = (n1.freeVarSeq, n1usage, n2usage).zipped.toSeq.map{
      tuple: (FreeVar, VarUsage, VarUsage) => tuple match {
        case (_, VarAllocated, VarAllocated) =>
          // Double allocation
          false
        case (_, VarUnused, _) =>
          // Only used in one of the objects => Don't need to have a name in both
          true
        case (_, _, VarUnused) =>
          // Only used in one of the objects => Don't need to have a name in both
          true
        case (v, used1, used2) =>
          assert(used1.isUsed && used2.isUsed)
          // Used in both => Need a common name
          (n1.rootParamSubst.get intersect n2.rootParamSubst.get).nonEmpty
      }
    }

    for {
      (v, unifiable) <- n1.freeVarSeq.zip(unifiableParams)
      if !unifiable
    } {
      logger.debug(s"Can't unify $v (among FVs ${n1.freeVarSeq}) in $n1 and $n2. (Shared non-placeholder name required but not present.)")
    }

    if (unifiableParams.forall(b => b)) {
      Some((n1.subst.toSeq, n2.subst.toSeq).zipped.map(_ union _))
    } else {
      None
    }
  }

  /* Note: Since we're using NodeLabels rather than e.g. NodeIDs here, we do not consider all compositions in the corner
     case that there are two leaves with the exact same node label. This doesn't matter for correctness though, since in
     such cases, it simply doesn't make a difference which leaf we replace, so it's not necessary to keep both
     candidates around.

     Note further that under the assumption that even base rules allocate memory, such objects will anyway always
     represent double allocation (same node label implies same root), so they should anyway be discarded.
   */
  case class CompositionInterface(treeToEmbed: EntailmentContext, embeddingTarget: EntailmentContext, leafToReplaceInEmbedding: PredicateNodeLabel)

  private def compositionCandidates(fst: EntailmentContext, snd: EntailmentContext): Stream[CompositionInterface] = {
    for {
      (treeWithRoot, treeWithAbstractLeaf) <- Stream((fst,snd), (snd,fst))
      root = treeWithRoot.root
      abstractLeaf <- treeWithAbstractLeaf.calls
      // Only consider for composition if the labeling predicates are the same
      if root.pred == abstractLeaf.pred
    } yield CompositionInterface(treeWithRoot, treeWithAbstractLeaf, abstractLeaf)
  }

  private def allMergeOptions(processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext]): Seq[Seq[EntailmentContext]] = {
    if (unprocessed.isEmpty) {
      Seq(processed)
    } else {
      optionalMerge(processed, unprocessed) flatMap {
        case (processedNew, unprocessedNew) => allMergeOptions(processedNew, unprocessedNew)
      }
    }
  }

  private def optionalMerge(processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext]): Seq[(Seq[EntailmentContext], Seq[EntailmentContext])] = {
    val (fst, other) = (unprocessed.head, unprocessed.tail)
    Seq((processed :+ fst, other)) ++ tryMerge(fst, other).map(pair => (processed, pair._1 +: pair._2))
  }

  @tailrec private def sweepingMerge(processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext]): Seq[EntailmentContext] = {
    if (unprocessed.isEmpty) {
      processed
    } else {
      tryMerge(unprocessed.head, unprocessed.tail) match {
        case Some((merged, other)) => sweepingMerge(processed, merged +: other)
        case None => sweepingMerge(processed :+ unprocessed.head, unprocessed.tail)
      }
    }
  }

  private def tryMerge(fst: EntailmentContext, other: Seq[EntailmentContext]): Option[(EntailmentContext, Seq[EntailmentContext])] = {
    (for {
      candidate <- other.toStream
      composed <- compose(fst, candidate)
    } yield (composed, other.filter(_ != candidate))).headOption
  }
  
}
