package at.forsyte.harrsh.seplog.sidtransformers

import scala.util.Try
import at.forsyte.harrsh.entailment.{EntailmentInstance, EntailmentQuerySide, TopLevelConstraint}
import at.forsyte.harrsh.main.{EntailmentQuery, HarrshLogging}
import at.forsyte.harrsh.seplog.inductive.{PredCall, Predicate, RichSid, SymbolicHeap}

object QueryToEntailmentInstance extends HarrshLogging {

  def apply(parseRes: EntailmentQuery, computeSeparateSidsForEachSide: Boolean, computeSccs: Boolean) : Try[EntailmentInstance] = {
    generalizedProgressNormalform(computeSeparateSidsForEachSide, computeSccs)(parseRes).map(logTransformationResult)
  }

  private def generalizedProgressNormalform(computeSeparateSidsForEachSide: Boolean, computeSccs: Boolean)(parseResult: EntailmentQuery): Try[EntailmentInstance] = {
    val introduceAuxPredsForPointers = !computeSccs

    for {
      rootedSid <- SidDirectionalityAnnotator(parseResult.sid)
      rootedWithOnePtoPerRule <- SplitMultiPointerRules(rootedSid)
      (sidWithAuxPreds, lhsAux, rhsAux) = {
        if (introduceAuxPredsForPointers) ReplacePointersByPredicates(rootedWithOnePtoPerRule, parseResult.lhs, parseResult.rhs)
        else (rootedWithOnePtoPerRule, parseResult.lhs, parseResult.rhs)
      }
      lhs = processEntailmentQuerySide(lhsAux, parseResult.lhs, sidWithAuxPreds, computeSeparateSidsForEachSide, computeSccs, isLhs = true)
      rhs = processEntailmentQuerySide(rhsAux, parseResult.rhs, sidWithAuxPreds, computeSeparateSidsForEachSide, computeSccs, isLhs = false)
    } yield EntailmentInstance(lhs, rhs, parseResult.status.toBoolean)
  }

  private def logTransformationResult(instance: EntailmentInstance): EntailmentInstance = {
    logger.debug(s"Will perform entailment check ${instance.queryString} (instead of ${instance.originalQueryString} w.r.t. SIDs in progress normal form:")
    logger.debug(s"LHS SID:\n${instance.lhs.sid}")
    logger.debug(s"RHS SID:\n${instance.rhs.sid}")
    instance
  }

  private def processEntailmentQuerySide(intermediateQuery: SymbolicHeap, originalQuerySide: SymbolicHeap, rootedSid: RichSid, computeSeparateSidsForEachSide: Boolean, computeSccs: Boolean, isLhs: Boolean): EntailmentQuerySide = {
    val sideSid = if (computeSeparateSidsForEachSide) RestrictSidToCalls(rootedSid, intermediateQuery.predCalls.toSet) else rootedSid

    val (processedSid, processedCalls) = if (computeSccs) {
       splitQueryIntoSccs(intermediateQuery, sideSid, isLhs)
    } else {
      (sideSid, TopLevelConstraint(intermediateQuery.predCalls, intermediateQuery.pure))
    }

    EntailmentQuerySide(processedSid, processedCalls, originalQuerySide)
  }

  private def splitQueryIntoSccs(querySide: SymbolicHeap, sid: RichSid, isLhs: Boolean): (RichSid, TopLevelConstraint) = {
    logger.debug(s"Will transform $querySide into one call per SCC, starting from SID\n$sid")
    ToSymbolicHeapOverBtwSid(querySide, if (isLhs) PrefixOfLhsAuxiliaryPreds else PrefixOfRhsAuxiliaryPreds, sid)
  }

}
