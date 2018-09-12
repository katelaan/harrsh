package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, Renaming, Var}

object SIDUtils extends HarrshLogging {

  // TODO: Introduce unique names for bound vars + don't hardcode prefix?
  private def defaultBoundVarNames(sh: SymbolicHeap): Seq[String] = sh.boundVars.toSeq.map(bv => "_"+bv.index)

  def shToRuleBody(sh: SymbolicHeap): RuleBody = {
    // TODO: The SH API is obviously not meant to be used in this way. Refactor?
    val withoutGaps = SymbolicHeap(sh.atoms.closeGapsInBoundVars, sh.freeVars)
    RuleBody(defaultBoundVarNames(withoutGaps), withoutGaps)
  }

  private def logTransformationResult(preds: Seq[Predicate]): Unit = {
    logger.debug(s"Transformation results:")
    for (pred <- preds) {
      logger.debug(s"Introducing pred ${pred.head} with free vars ${pred.rules.head.body.freeVars}, atoms ${pred.rules.head.body.atoms}, body ${pred.rules.head.body}")
      logger.debug(s"Overall: $pred")
    }
  }

  def shToProgressSid(sh: SymbolicHeap, predPrefix: String, underlyingSID: SID): SID = {
    val TransformationResult(headPred, otherPreds) = introduceOnePredPerPointer(sh, predPrefix)
    logTransformationResult(headPred +: otherPreds)
    val normalizedHeadPred = normalizeHeadPred(headPred, underlyingSID)
    SID(normalizedHeadPred.head, normalizedHeadPred +: otherPreds, s"Progress normal form of $sh")
  }

  private def normalizeHeadPred(headPred: Predicate, underlyingSID: SID): Predicate = {
    assert(headPred.rules.size == 1)
    val rule = headPred.rules.head
    val body = rule.body
    if (body.hasPointer || body.predCalls.isEmpty) {
      // Progress satisfied => no further normalization necessary
      headPred
    } else {
      // Extract rules of an arbitrary pred call to establish progress
      val fstCall = body.predCalls.head
      val ruleBodies = underlyingSID(fstCall.name).bodySHs
      val instantiatedBodies = ruleBodies map (body.replaceCall(fstCall, _))
      val newRules = instantiatedBodies map shToRuleBody
      val res = Predicate(headPred.head, newRules, headPred.rootParam)
      logger.debug(s"Normalizing rule ${headPred.head} <= $rule by replacing $fstCall with rules of ${fstCall.name}:\n$res")
      res
    }
  }

  case class TransformationResult(headPredicate: Predicate, otherPredicates: Seq[Predicate])

  private def introduceOnePredPerPointer(intermediateSh: SymbolicHeap, predPrefix: String, introducedPredicates: Seq[Predicate] = Seq.empty): TransformationResult = {
    val currName = predPrefix + (introducedPredicates.size + 1)
    val successorName = predPrefix + (introducedPredicates.size + 2)
    intermediateSh.pointers.size match {
      case 0 =>
        // No local allocation at all;
        // Simply introduce a single new predicate for this
        // Note that this will lead to special treatment in the automaton transitions, because progress is violated
        assert(introducedPredicates.isEmpty)
        TransformationResult(Predicate(currName, Seq(shToRuleBody(intermediateSh))), Seq.empty)
      case 1 =>
        // Exactly one pointer => Progress is satisfied => Simply create a single predicate
        val allPreds = introducedPredicates :+ Predicate(currName, Seq(shToRuleBody(intermediateSh)))
        TransformationResult(allPreds.head, allPreds.tail)
      case n if n >= 2 =>
        // Recurse on an arbitrary pointer
        val localPtr = intermediateSh.pointers.head
        val SplitResult(remainder, droppedFreeVars, newFreeVarsByBoundVar) = splitOffPtrAndAllPureAtoms(localPtr, intermediateSh)
        val recPredName = successorName
        val recPredParams = intermediateSh.freeVars.filterNot(droppedFreeVars.contains) ++ newFreeVarsByBoundVar.map(_._1)
        val recPredCall = PredCall(recPredName, recPredParams)
        val ruleSh = SymbolicHeap(pure = intermediateSh.pure,
          pointers = Seq(localPtr),
          predCalls = Seq(recPredCall),
          freeVars = intermediateSh.freeVars)
        val newPred = Predicate(currName, Seq(shToRuleBody(ruleSh)))
        introduceOnePredPerPointer(remainder, predPrefix, introducedPredicates :+ newPred)
    }
  }

  private case class SplitResult(newSh: SymbolicHeap, droppedFreeVars: Set[FreeVar], newFreeVarsByBoundVar: Seq[(BoundVar,Var)])

  /**
    * Return sh after removing atom. Any bound var shared between ptr/pure atoms and sh is turned into a fresh free var.
    */
  private def splitOffPtrAndAllPureAtoms(ptr: PointsTo, sh: SymbolicHeap): SplitResult = {
    val pureAtoms = sh.pure
    val splitOffBoundVars = Var.boundVars(ptr.getNonNullVars ++ pureAtoms.flatMap(_.getNonNullVars))
    val remainderAtoms = sh.pointers.filterNot(_ == ptr) ++ sh.predCalls
    val remainder = SymbolicHeap(remainderAtoms:_*)

    // All bound vars that are shared between the two parts must become parameters of the new predicate...
    val sharedBoundVars = (splitOffBoundVars intersect remainder.boundVars).toSeq
    // ...whereas free vars that don't occur in the part of the splitting won't become parameters of the new predicate...
    val droppedFreeVars = sh.freeVars.filterNot(remainder.freeVars.contains).toSet
    // ...leading to the following renaming of free/bound vars to the new uninterrupted sequence of free vars:
    val freeVarNamesForRemainder = Var.freshFreeVars(Set.empty, remainder.freeVars.size + sharedBoundVars.size)
    val renamingMap: Map[Var,Var] = (remainder.freeVars ++ sharedBoundVars, freeVarNamesForRemainder).zipped.toMap
    val renaming = Renaming.fromMap(renamingMap)
    val renamedRemainder = remainder.rename(renaming, overrideFreeVars = Some(freeVarNamesForRemainder))

    // Close gaps in resulting bound variable sequence
    // TODO: The SH API is obviously not meant to be used in this way. Refactor?
    val normalizedRemainder = SymbolicHeap(renamedRemainder.atoms.closeGapsInBoundVars, renamedRemainder.freeVars)

    logger.debug(s"Splitting off $ptr and pure atoms from $sh: Got $remainder, renamed to $renamedRemainder, normalized to $normalizedRemainder")
    val newFreeVarsByBoundVar = sharedBoundVars.map(bv => (bv,renamingMap(bv)))
    SplitResult(normalizedRemainder, droppedFreeVars, newFreeVarsByBoundVar)
  }

}
