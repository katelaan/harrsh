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
      logger.debug(s"Trying to merge $shiftedFst with $shiftedSnd, but they both allocate $sharedAllocation => Can't compose")
      None
    }
    else {
      logger.trace(s"Decompositions after shifting:\n$shiftedFst and\n$shiftedSnd")

      val mergeNondisjointVarLabelSets = MergeUpdate(shiftedFst.constraints.classes, shiftedSnd.constraints.classes)
      val maybeUnion = mergeWithUnifyingUpdate(shiftedFst, shiftedSnd, mergeNondisjointVarLabelSets)

      maybeUnion match {
        case Some(union) => logger.debug("Union decomposition (on which merge options will be computed):\n" + union)
        case None => logger.debug("Union is undefined => Discarding decomposition.")
      }

      maybeUnion
    }
  }

  private def mergeWithUnifyingUpdate(fst: ContextDecomposition, snd: ContextDecomposition, upd: MergeUpdate): Option[ContextDecomposition] = {
    val mergedParts = (fst.parts ++ snd.parts).map(_.updateSubst(upd))
    for {
      mergedConstraints <- MergeUpdate.mergeUsingUpdate(fst.constraints, snd.constraints, upd)
    } yield ContextDecomposition(mergedParts, mergedConstraints)
  }

  private def makePlaceholdersDisjoint(fst: ContextDecomposition, snd: ContextDecomposition): (ContextDecomposition, ContextDecomposition) = {
    val clashAvoidanceUpdate = PlaceholderVar.placeholderClashAvoidanceUpdate(snd.typedPlaceholders)
    (fst.updateSubst(clashAvoidanceUpdate).get, snd)
  }

  private def allMergeOptions(sid: RichSid, unionDecomp: ContextDecomposition): Seq[ContextDecomposition] = {
    allMergeOptions(sid, Seq.empty, unionDecomp.parts.toSeq, unionDecomp.constraints)
  }

  private def allMergeOptions(sid: RichSid, processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext], constraints: VarConstraints): Seq[ContextDecomposition] = {
    if (unprocessed.isEmpty) {
      val occurringVarSets = processed.toSet[EntailmentContext].flatMap(_.labels).flatMap(_.subst.toSeq)
      val placeholders = occurringVarSets.flatten.filter(PlaceholderVar.isPlaceholder)
      val maybeRes = for {
        cleanedConstraints <- constraints.restrictPlaceholdersTo(placeholders)
        composed = ContextDecomposition(processed.toSet, cleanedConstraints)
        res = composed.toPlaceholderNormalForm
      } yield res
      maybeRes match {
        case None =>
          logger.debug(s"Merging failed because placeholders were used in speculation in $constraints of processed contexts\n${processed.mkString("\n")}")
          Seq.empty
        case Some(res) =>
          logger.debug(s"New merge result: $res")
          Seq(res)
      }
    } else {
      for {
        (nowProcessed, stillUnprocessed) <- optionalMerge(sid, processed, unprocessed, constraints)
        merged <- allMergeOptions(sid, nowProcessed, stillUnprocessed, constraints)
      } yield merged
    }
  }

  private def optionalMerge(sid: RichSid, processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext], constraints: VarConstraints): Seq[(Seq[EntailmentContext], Seq[EntailmentContext])] = {
    val (fst, other) = (unprocessed.head, unprocessed.tail)
    (
      // Don't merge fst with anything, just add to processed
      Seq((processed :+ fst, other))
        ++ tryMerge(sid, fst, other, constraints).map(t => (processed, t._1 +: t._2))
      )
  }

  private def tryMerge(sid: RichSid, fst: EntailmentContext, other: Seq[EntailmentContext], constraints: VarConstraints): Stream[(EntailmentContext, Seq[EntailmentContext], ConstraintUpdater)] = {
    for {
      candidate <- other.toStream
      _ = logger.debug(s"Will try to compose $fst with $candidate wrt constraints $constraints.")
      ((composed, updater), i) <- EntailmentContextComposition(sid, fst, candidate, constraints).zipWithIndex
      _ = logger.debug(s"Composition success #${i+1}: Composed context $composed\nusing updater $updater")
    } yield (composed, other.filter(_ != candidate), updater)
  }

//  private def allMergeOptions(sid: RichSid, processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext], constraints: VarConstraints): Seq[ContextDecomposition] = {
//    if (unprocessed.isEmpty) {
//      logger.debug(s"At the end of the merge process:\n$processed")
//      // Since the constraints are updated context by context, not all processed contexts will already reflect the new constraints.
//      // We thus perform another unification step.
//      val upd = PropagationUpdate(constraints.classes)
//      val processedAndPropagated = processed map (_.updateSubst(upd))
//      logger.debug(s"After propagation of constraints:\n$processedAndPropagated")
//
//      val occurringVarSets = processedAndPropagated.toSet[EntailmentContext].flatMap(_.labels).flatMap(_.subst.toSeq)
//      val placeholders = occurringVarSets.flatten.filter(PlaceholderVar.isPlaceholder)
//      val cleanedConstraints = constraints.restrictPlaceholdersTo(placeholders)
//
//      val composed = ContextDecomposition(processedAndPropagated.toSet, cleanedConstraints)
//      val res = Seq(composed.toPlaceholderNormalForm)
//      logger.debug(s"New merge result: $res")
//      res
//    } else {
//      for {
//        (nowProcessed, stillUnprocessed, newConstraints, variableMergingImposedByComposition) <- optionalMerge(sid, processed, unprocessed, constraints)
//        unprocessedWithMergedVars = stillUnprocessed.map(_.updateSubst(variableMergingImposedByComposition))
//        merged <- allMergeOptions(sid, nowProcessed, unprocessedWithMergedVars, newConstraints)
//      } yield merged
//    }
//  }

//  private def optionalMerge(sid: RichSid, processed: Seq[EntailmentContext], unprocessed: Seq[EntailmentContext], constraints: VarConstraints): Seq[(Seq[EntailmentContext], Seq[EntailmentContext], VarConstraints, SubstitutionUpdate)] = {
//    val (fst, other) = (unprocessed.head, unprocessed.tail)
//    (
//      // Don't merge fst with anything, just add to processed
//      Seq((processed :+ fst, other, constraints, SubstitutionUpdateMap(Map.empty)))
//        ++ tryMerge(sid, fst, other, constraints).map(t => (processed, t._1 +: t._2, t._3, t._4))
//      )
//  }
//
//  private def tryMerge(sid: RichSid, fst: EntailmentContext, other: Seq[EntailmentContext], constraints: VarConstraints): Stream[(EntailmentContext, Seq[EntailmentContext], VarConstraints, SubstitutionUpdate)] = {
//    for {
//      candidate <- other.toStream
//      _ = logger.debug(s"Will try to compose $fst with $candidate wrt constraints $constraints.")
//      ((composed, newConstraints, variableMergingImposedByComposition), i) <- EntailmentContextComposition(sid, fst, candidate, constraints).zipWithIndex
//      _ = logger.debug(s"Composition success #${i+1}: Composed context $composed with constraints $newConstraints")
//    } yield (composed, other.filter(_ != candidate), newConstraints, variableMergingImposedByComposition)
//  }

}
