package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.entailment.{EntailmentInstance, EntailmentQuerySide, PredCalls}
import at.forsyte.harrsh.main.{EntailmentQuery, HarrshLogging}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RichSid, SymbolicHeap}

import scala.util.{Failure, Success, Try}

object QueryToEntailmentInstance extends HarrshLogging {

  def apply(parseRes: EntailmentQuery, computeSeparateSidsForEachSide: Boolean) : Option[EntailmentInstance] = {
    generalizedProgressNormalform(computeSeparateSidsForEachSide)(parseRes).map(logTransformationResult) match {
      case Success(ei) => Some(ei)
      case Failure(e) =>
        logger.error("Conversion to entailment instance failed with exception: " + e.getMessage)
        //e.printStackTrace()
        None
    }
  }

  private def generalizedProgressNormalform(computeSeparateSidsForEachSide: Boolean)(parseResult: EntailmentQuery): Try[EntailmentInstance] = {
    for {
      rootedSid <- AnnotateSidWithRootParams(parseResult.sid)
      rootWithOnePtoPerRule <- SplitMultiPointerRules(rootedSid)
      if satisfiesGeneralizedProgress(rootWithOnePtoPerRule)
      lhs = processEntailmentQuerySide(parseResult.lhs, rootWithOnePtoPerRule, computeSeparateSidsForEachSide, isLhs = true)
      rhs = processEntailmentQuerySide(parseResult.rhs, rootWithOnePtoPerRule, computeSeparateSidsForEachSide, isLhs = false)
    } yield EntailmentInstance(lhs, rhs, parseResult.status.toBoolean)
  }

  private def logTransformationResult(instance: EntailmentInstance): EntailmentInstance = {
    logger.debug(s"Will perform entailment check ${instance.queryString} (instead of ${instance.originalQueryString} w.r.t. SIDs in progress normal form:")
    logger.debug(s"LHS SID:\n${instance.lhs.sid}")
    logger.debug(s"RHS SID:\n${instance.rhs.sid}")
    instance
  }

  private def satisfiesGeneralizedProgress(sid: RichSid): Boolean = {
    if (!sid.satisfiesGeneralizedProgress)
      logger.error(s"Discarding input because the SID $sid does not satisfy progress; violating rules: ${sid.rulesViolatingProgress.map(_._2)}")
    sid.satisfiesGeneralizedProgress
  }

  private def processEntailmentQuerySide(originalQuerySide: SymbolicHeap, rootedSid: RichSid, computeSeparateSidsForEachSide: Boolean, isLhs: Boolean): EntailmentQuerySide = {
    val sideSid = if (computeSeparateSidsForEachSide) RestrictSidToCalls(rootedSid, originalQuerySide.predCalls.toSet) else rootedSid
    // TODO: Make splitting into SCCs optional + more explicit in the EI at type level!
    val (sccSid, lhsCalls) = splitQueryIntoSccs(originalQuerySide, sideSid, isLhs)
    EntailmentQuerySide(sccSid, lhsCalls, originalQuerySide)
  }

  private def splitQueryIntoSccs(querySide: SymbolicHeap, sid: RichSid, isLhs: Boolean): (RichSid, PredCalls) = {
    logger.debug(s"Will transform $querySide into one call per SCC, starting from SID\n$sid")
    ToSymbolicHeapOverBtwSid(querySide, if (isLhs) PrefixOfLhsAuxiliaryPreds else PrefixOfRhsAuxiliaryPreds, sid)
  }

}
