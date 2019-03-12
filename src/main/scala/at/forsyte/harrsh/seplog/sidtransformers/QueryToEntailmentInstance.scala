package at.forsyte.harrsh.seplog.sidtransformers

import scala.util.Try

import at.forsyte.harrsh.entailment.{EntailmentInstance, EntailmentQuerySide, PredCalls}
import at.forsyte.harrsh.main.{EntailmentQuery, HarrshLogging}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RichSid, SymbolicHeap}

object QueryToEntailmentInstance extends HarrshLogging {

  def apply(parseRes: EntailmentQuery, computeSeparateSidsForEachSide: Boolean, computeSccs: Boolean) : Try[EntailmentInstance] = {
    generalizedProgressNormalform(computeSeparateSidsForEachSide, computeSccs)(parseRes).map(logTransformationResult)
  }

  private def generalizedProgressNormalform(computeSeparateSidsForEachSide: Boolean, computeSccs: Boolean)(parseResult: EntailmentQuery): Try[EntailmentInstance] = {
    for {
      rootedSid <- SidDirectionalityAnnotator(parseResult.sid)
      rootWithOnePtoPerRule <- SplitMultiPointerRules(rootedSid)
      lhs = processEntailmentQuerySide(parseResult.lhs, rootWithOnePtoPerRule, computeSeparateSidsForEachSide, computeSccs, isLhs = true)
      rhs = processEntailmentQuerySide(parseResult.rhs, rootWithOnePtoPerRule, computeSeparateSidsForEachSide, computeSccs, isLhs = false)
    } yield EntailmentInstance(lhs, rhs, parseResult.status.toBoolean)
  }

  private def logTransformationResult(instance: EntailmentInstance): EntailmentInstance = {
    logger.debug(s"Will perform entailment check ${instance.queryString} (instead of ${instance.originalQueryString} w.r.t. SIDs in progress normal form:")
    logger.debug(s"LHS SID:\n${instance.lhs.sid}")
    logger.debug(s"RHS SID:\n${instance.rhs.sid}")
    instance
  }

  private def processEntailmentQuerySide(originalQuerySide: SymbolicHeap, rootedSid: RichSid, computeSeparateSidsForEachSide: Boolean, computeSccs: Boolean, isLhs: Boolean): EntailmentQuerySide = {
    val sideSid = if (computeSeparateSidsForEachSide) RestrictSidToCalls(rootedSid, originalQuerySide.predCalls.toSet) else rootedSid
    val (processedSid, processedCalls) = if (computeSccs) {
       splitQueryIntoSccs(originalQuerySide, sideSid, isLhs)
    } else {
      (sideSid, PredCalls(originalQuerySide.predCalls))
    }
    EntailmentQuerySide(processedSid, processedCalls, originalQuerySide)
  }

  private def splitQueryIntoSccs(querySide: SymbolicHeap, sid: RichSid, isLhs: Boolean): (RichSid, PredCalls) = {
    logger.debug(s"Will transform $querySide into one call per SCC, starting from SID\n$sid")
    ToSymbolicHeapOverBtwSid(querySide, if (isLhs) PrefixOfLhsAuxiliaryPreds else PrefixOfRhsAuxiliaryPreds, sid)
  }

}
