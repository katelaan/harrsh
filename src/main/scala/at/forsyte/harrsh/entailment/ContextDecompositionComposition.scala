package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.RichSid

object ContextDecompositionComposition extends HarrshLogging {

  def apply(sid: RichSid, fst: ContextDecomposition, snd: ContextDecomposition): Seq[ContextDecomposition] = {
    logger.debug(s"Will compose decompositions\n$fst and\n$snd")
    for {
      union <- unionWithoutMerges(fst, snd).toSeq
      merged <- allMergeOptions(sid, union)
      _ = assert(merged.isInPlaceholderNormalForm, s"Composition of decomposition failed to re-establish normal form: $merged")
    } yield merged
  }

  private def unionWithoutMerges(fst: ContextDecomposition, snd: ContextDecomposition): Option[ContextDecomposition] = {
    val (shiftedFst, shiftedSnd) = makePlaceholdersDisjoint(fst, snd)
    val sharedAllocation = shiftedFst.allocedVars.intersect(shiftedSnd.allocedVars)
    if (sharedAllocation.nonEmpty) {
      throw DoubleAllocException(s"Trying to merge $shiftedFst with $shiftedSnd, but they both allocate $sharedAllocation")
    }
    logger.trace(s"Decompositions after shifting:\n$shiftedFst and\n$shiftedSnd")

    // TODO: We should probably catch DoubleAllocException here and then return None (because speculative pure constraints (the `missing` part) can likely lead to double allocation even for ALL-SAT SIDs) but for now I want to see the exceptions
    val mergeNondisjointVarLabelSets = SubstitutionUpdate.unionUpdate(shiftedFst.usageInfo, shiftedSnd.usageInfo)
    val union = mergeWithUnifyingUpdate(shiftedFst, shiftedSnd, mergeNondisjointVarLabelSets)
    logger.debug("Union decomposition (on which merge options will be computed):\n" + union)
    if (union.pureConstraints.isConsistent && !union.explicitlyAllocsNull) {
      Some(union)
    } else {
      logger.debug("Union is inconsistent => Discarding decomposition.")
      None
    }
  }

  private def mergeWithUnifyingUpdate(fst: ContextDecomposition, snd: ContextDecomposition, upd: SubstitutionUpdate): ContextDecomposition = {
    val mergedParts = (fst.parts ++ snd.parts).map(_.updateSubst(upd))
    val mergedUsageInfo = VarUsageByLabel.combineUsageInfo(fst.usageInfo, snd.usageInfo, upd)
    val mergedPure = (fst.pureConstraints ++ snd.pureConstraints).update(upd)
    ContextDecomposition(mergedParts, mergedUsageInfo, mergedPure)
  }

  private def makePlaceholdersDisjoint(fst: ContextDecomposition, snd: ContextDecomposition): (ContextDecomposition, ContextDecomposition) = {
    val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.placeholders)
    (fst.updateSubst(clashAvoidanceUpdate), snd)
  }

  private def allMergeOptions(sid: RichSid, unionDecomp: ContextDecomposition): Seq[ContextDecomposition] = {
    allMergeOptions(sid, Seq.empty, unionDecomp.parts.toSeq, unionDecomp.usageInfo, unionDecomp.pureConstraints)
  }

  private def allMergeOptions(sid: RichSid, processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker): Seq[ContextDecomposition] = {
    assert(VarUsageByLabel.isWellFormed(usageInfo), "Overlapping entries in usage info: " + usageInfo)

    if (unprocessed.isEmpty) {
      logger.debug(s"At the end of the merge process:\n$processed")
      // Since the usage info is updated context by context, not all processed contexts will already reflect the new usage info.
      // We thus perform another unification step.
      val upd = SubstitutionUpdate.fromUnification(usageInfo.keys.toSeq)
      val processedAndPropagated = processed map (_.updateSubst(upd))
      logger.debug(s"After propagation of usage info:\n$processedAndPropagated")
      val propagatedPureConstraints = pureConstraints.update(upd)
      val cleanedUsageInfo = VarUsageByLabel.restrictToOccurringLabels(usageInfo, processedAndPropagated)
      logger.debug("Cleaning usage info: " + usageInfo + " into " + cleanedUsageInfo)
      val composed = ContextDecomposition(processedAndPropagated.toSet, cleanedUsageInfo, propagatedPureConstraints)
      val res = Seq(composed.toPlaceholderNormalForm)
      logger.debug(s"New merge result: $res")
      res
    } else {
      for {
        (nowProcessed, stillUnprocessed, newUsageInfo, newPureConstraints) <- optionalMerge(sid, processed, unprocessed, usageInfo, pureConstraints)
        merged <- allMergeOptions(sid, nowProcessed, stillUnprocessed, newUsageInfo, newPureConstraints)
      } yield merged
    }
  }

  private def optionalMerge(sid: RichSid, processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker): Seq[(Seq[EntailmentContext], Seq[EntailmentContext], VarUsageByLabel, PureConstraintTracker)] = {
    val (fst, other) = (unprocessed.head, unprocessed.tail)
    (
      // Don't merge fst with anything, just add to processed
      Seq((processed :+ fst, other, usageInfo, pureConstraints))
        ++ tryMerge(sid, fst, other, usageInfo, pureConstraints).map(t => (processed, t._1 +: t._2, t._3, t._4))
      )
  }

  private def tryMerge(sid: RichSid, fst: EntailmentContext, other: Seq[EntailmentContext], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker): Stream[(EntailmentContext, Seq[EntailmentContext], VarUsageByLabel, PureConstraintTracker)] = {
    for {
      candidate <- other.toStream
      _ = logger.debug(s"Will try to compose $fst with $candidate wrt usage $usageInfo and pure constraints $pureConstraints.")
      ((composed, newUsage, newPureConstraints), i) <- EntailmentContextComposition(sid, fst, candidate, usageInfo, pureConstraints).zipWithIndex
      _ = logger.debug(s"Composition success #${i+1}: Composed context $composed with usage $newUsage and pure constraints $newPureConstraints")
    } yield (composed, other.filter(_ != candidate), newUsage, newPureConstraints)
  }

}
