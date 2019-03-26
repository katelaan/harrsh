package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.{EmptyPredicates, PureAtom, RichSid}
import at.forsyte.harrsh.util.Combinators

case class EmpClosure(sid: RichSid) extends HarrshLogging {

  val predsWithEmptyModels: EmptyPredicates = sid.predsWithEmptyModels

  def apply(profile: EntailmentProfile): EntailmentProfile = profile.applyToDecomps {
    decomps =>
      logger.debug("Will compute emp-closure")
      decomps.flatMap(empClosureOfDecomp)
  }

  def empClosureOfDecomp(decomp: ContextDecomposition): Set[ContextDecomposition] = {
    val empClosureByCtx: Seq[Set[(EntailmentContext, Set[PureAtom])]] = decomp.parts.toSeq map empClosureOfContext
    for {
      closureOption: Seq[(EntailmentContext, Set[PureAtom])] <- Combinators.choices(empClosureByCtx)
      _ = logger.debug(s"Considering emp-closure for $decomp:\nWill update contexts as follows:\n${closureOption.map(p => p._1 + " with new pure constraints " + p._2).mkString(",\n")}")
      (newCtxs, pureConstraintsByCtx) = closureOption.unzip
      newPureAtoms = pureConstraintsByCtx.flatten
      pureAtomUpdate = SpeculativeUpdate(newPureAtoms, decomp.constraints.classes)
      withNewAtoms <- pureAtomUpdate(decomp.constraints)
      updatedCtxs = newCtxs.map(_.updateSubst(pureAtomUpdate)).toSet
      // Get rid of placeholders that occurred only in the call(s) which we removed
      leftoverPlaceholders = updatedCtxs.flatMap(_.placeholders)
      cleanedConstraints <- withNewAtoms.restrictPlaceholdersTo(leftoverPlaceholders)
      // Get rid of redundant placeholders
      placeholderDropper = DropperUpdate(updatedCtxs.flatMap(_.redundantPlaceholders))
      finalCtxs = updatedCtxs map (_.updateSubst(placeholderDropper))
      finalConstraints <- placeholderDropper(cleanedConstraints)
      newDecomp = ContextDecomposition(finalCtxs, finalConstraints)

      if newDecomp.isConsistentWithFocus(sid)
      res = newDecomp.toPlaceholderNormalForm
      _ = logger.debug("Emp-closure is consistent. Will retain updated decomposition\n" + res)
    } yield res
  }

  private def empClosureOfContext(ctx: EntailmentContext): Set[(EntailmentContext, Set[PureAtom])] = {
    for {
      (callsAfterClosure, constraintsOfClosure) <- empClosureOfCalls(ctx.calls)
    } yield (EntailmentContext(ctx.root, callsAfterClosure), constraintsOfClosure)
  }

  private def empClosureOfCalls(calls: Set[ContextPredCall], remainingCalls: Set[ContextPredCall] = Set.empty, constraintsSoFar: Set[PureAtom] = Set.empty) : Set[(Set[ContextPredCall], Set[PureAtom])] = {
    if (calls.isEmpty) {
      Set((remainingCalls, constraintsSoFar))
    } else {
      val (hd, tl) = (calls.head, calls.tail)
      empClosureOfCalls(tl, remainingCalls + hd, constraintsSoFar) ++
        empClosureAfterRemoval(hd, tl, remainingCalls, constraintsSoFar)
    }
  }

  private def empClosureAfterRemoval(callToRemove: ContextPredCall, unprocessedCalls: Set[ContextPredCall], remainingCalls: Set[ContextPredCall], constraintsSoFar: Set[PureAtom]) : Set[(Set[ContextPredCall], Set[PureAtom])] = {
    if (sid.hasEmptyModels(callToRemove.pred)) {
      val constraintOptions = constraintOptionsForCall(callToRemove)
      val closureOfUnprocessedCalls = empClosureOfCalls(unprocessedCalls, remainingCalls, constraintsSoFar)
      for {
        (remainingAfterClosure, constraintsAfterClosure) <- closureOfUnprocessedCalls
        constraint <- constraintOptions
      } yield (remainingAfterClosure, constraintsAfterClosure ++ constraint)
    } else {
      Set.empty
    }
  }

  private def constraintOptionsForCall(call: ContextPredCall) = {
    val update: Map[Var, Var] = ((NullConst, NullConst) +: (call.freeVarSeq zip call.subst.toSeq.map(_.head))).toMap
    for {
      option <- sid.constraintOptionsForEmptyModels(call.pred)
    } yield option.map(atom => PureAtom(update(atom.l), update(atom.r), atom.isEquality))
  }


  /**
    * Convert decomp into concrete decomp by dropping empty leaves (if possible).
    *
    * If not all leaves can be empty, returns None.
   */
  def makeConcrete(decomp: ContextDecomposition): Option[Set[EntailmentContext]] = {
    val ContextDecomposition(parts, constraints) = decomp
    if (predsWithEmptyModels.nonEmpty) {
      makeConcrete(parts, constraints)
    } else {
      if (parts forall (_.isConcrete)) Some(parts) else None
    }
  }

  private def makeConcrete(parts: Set[EntailmentContext], constraints: VarConstraints): Option[Set[EntailmentContext]] = {
    if (parts.isEmpty) Some(parts)
    else {
      val (hd, tl) = (parts.head, parts.tail)
      for {
        concrete <- makeConcrete(hd, constraints)
        concreteTl <- makeConcrete(tl, constraints)
      } yield concreteTl + concrete
    }
  }

  def makeConcrete(ctx: EntailmentContext, constraints: VarConstraints): Option[EntailmentContext] = {
    if (ctx.calls forall (canBeEmpty(_, constraints))) {
      Some(ctx.copy(calls = Set.empty))
    } else {
      None
    }
  }

  private def canBeEmpty(predCall: ContextPredCall, constraints: VarConstraints): Boolean = {
    if (predsWithEmptyModels.hasEmptyModels(predCall.pred.head)) {
      logger.debug(s"Check whether $predCall can be interpreted by empty model under the constraints $constraints")
      val options = predsWithEmptyModels(predCall)
      options find (pure => constraints.impliesWithoutSpeculation(pure)) match {
        case None =>
          logger.debug(s"Constraints $constraints don't imply ${options.mkString(" or ")}")
          false
        case Some(option) =>
          logger.debug(s"$constraints implies $option => Can remove $predCall from leaves")
          true
      }
    } else {
      logger.debug(s"$predCall can't have empty models => Cannot remove call from leaves")
      false
    }
  }

}
