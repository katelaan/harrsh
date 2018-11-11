package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentInstance
import at.forsyte.harrsh.main.{HarrshLogging, MainIO}
import at.forsyte.harrsh.parsers.EntailmentParser.EntailmentParseResult
import at.forsyte.harrsh.parsers.buildingblocks.{AsciiAtoms, EmptyQuantifierPrefix}
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.IOUtils

import scala.util.{Failure, Success, Try}

object EntailmentParsers extends HarrshLogging {

  val DefaultEntailmentParser = new EntailmentParser with HarrshSIDParser with AsciiAtoms with EmptyQuantifierPrefix

  val PrefixOfLhsAuxiliaryPreds = "lhs"
  val PrefixOfRhsAuxiliaryPreds = "rhs"

  def isAuxiliaryPred(pred: Predicate): Boolean = pred.head.startsWith(PrefixOfLhsAuxiliaryPreds) || pred.head.startsWith(PrefixOfRhsAuxiliaryPreds)

  def fileToEntailmentInstance(file: String, computeSidsForEachSideOfEntailment: Boolean): Option[EntailmentInstance] = {
    if (file.endsWith(MainIO.FileExtensions.SlComp)) {
      slcomp.parseFileToEntailmentInstance(file, computeSidsForEachSideOfEntailment)
    } else {
      val fileContent = IOUtils.readFile(file)
      parseHarrshEntailmentFormat(fileContent, computeSidsForEachSideOfEntailment)
    }
  }

  def parseHarrshEntailmentFormat(input: String, computeSeparateSidsForEachSide: Boolean): Option[EntailmentInstance] = {
    DefaultEntailmentParser.run(input) flatMap (res => normalize(res, computeSeparateSidsForEachSide))
  }

  def normalize(parseRes: EntailmentParseResult, computeSeparateSidsForEachSide: Boolean) : Option[EntailmentInstance] = {
    establishProgress(computeSeparateSidsForEachSide)(parseRes) map logTransformationResult
  }

  private def logTransformationResult(instance: EntailmentInstance): EntailmentInstance = {
    logger.debug(s"Will perform entailment check ${instance.lhsCall} |= ${instance.rhsCall} w.r.t. SIDs in progress normal form:")
    logger.debug(s"LHS SID:\n${instance.lhsSid}")
    logger.debug(s"RHS SID:\n${instance.rhsSid}")
    instance
  }

  private def establishProgress(computeSeparateSidsForEachSide: Boolean)(parseResult: EntailmentParser.EntailmentParseResult): Option[EntailmentInstance] = {
    for {
      rootedSid <- makeRooted(parseResult.sid)
      if satisfiesProgress(rootedSid)
      (lhsSid, lhsCall) <- establishProgress(parseResult.lhs, rootedSid, isLhs = true, computeSeparateSidsForEachSide)
      (rhsSid, rhsCall) <- establishProgress(parseResult.rhs, rootedSid, isLhs = false, computeSeparateSidsForEachSide)
    } yield EntailmentInstance(lhsSid, lhsCall, rhsSid, rhsCall, parseResult.entailmentHolds)
  }

  private def establishProgress(querySide: SymbolicHeap, sid: SID, isLhs: Boolean, computeSeparateSidsForEachSide: Boolean): Option[(SID, PredCall)] = {
    // If the query is a single predicate call, it is not necessary to introduce a new predicate,\
    // unless the call references bound variables or null
    val withoutNormalization = asSingleCallQuery(querySide, sid, isLhs, computeSeparateSidsForEachSide)

    if (withoutNormalization.isDefined) {
      withoutNormalization
    } else {
      normalizeToSingleCallQuery(querySide, sid, isLhs)
    }
  }

  private def asSingleCallQuery(querySide: SymbolicHeap, sid: SID, isLhs: Boolean, computeSeparateSidsForEachSide: Boolean): Option[(SID, PredCall)] = {
    for {
      call <- toSingleCall(querySide)
      if hasCorrectArity(call, sid)
      if call.args.forall(_.isFreeNonNull)
      // Without normalization, parameter reordering is only allowed on the RHS
      // (Parameter reordering on the RHS is possible, because the parameter order is part of the final state test.)
      if !isLhs || call.args == sid(call.name).params
      querySid <- if (computeSeparateSidsForEachSide) extractSidForCall(sid, call) else Some(sid)
    } yield (querySid, call)
  }

  private def normalizeToSingleCallQuery(querySide: SymbolicHeap, sid: SID, isLhs: Boolean): Option[(SID, PredCall)] = {
    logger.debug(s"Will establish progress normal form for $querySide for SID\n$sid")
    val queryPreds = SIDUtils.shToProgressSid(querySide, if (isLhs) PrefixOfLhsAuxiliaryPreds else PrefixOfRhsAuxiliaryPreds, sid)
    for {
      querySid <- combineIntoSidForSide(sid, queryPreds)
      call = querySid.callToStartPred.predCalls.head
    } yield (querySid, call)
  }

  private def hasCorrectArity(call: PredCall, sid: SID) = {
    val res = call.args.length == sid(call.name).arity
    if (!res) {
      logger.warn(s"Invalid input: Query contains call $call, but predicate ${call.name} has arity ${sid(call.name).arity}")
    }
    res
  }

  private def makePredRooted(pred: Predicate): Predicate = {
    pred.rules.find(!_.satisfiesGeneralizedProgress(rootOfPred = None)).map {
      rule => throw new IllegalArgumentException(s"SID contains a rule that violates progress: $rule")
    }

    val rootVars = sourcesOfHeadPtrs(pred)
    if (rootVars.size == 1) pred.copy(rootParam = rootVars.headOption.map(_.asInstanceOf[FreeVar]))
    else throw new IllegalArgumentException(s"No unique root parameter in predicate $pred; roots: $rootVars")
  }

  private def sourcesOfHeadPtrs(pred: Predicate): Set[Var] = {
    pred.rules.flatMap(_.body.pointers.headOption.map(_.from)).toSet
  }

  private def makeRooted(sid: SID): Option[SID] = {
    Try {
      val rootedPreds = sid.preds map makePredRooted
      sid.copy(preds = rootedPreds)
    } match {
      case Failure(exception) =>
        logger.warn(s"Can't annotate SID with root parameters: ${exception.getMessage}")
        None
      case Success(annotatedSID) => Some(annotatedSID)
    }
  }

  private def satisfiesProgress(sid: SID): Boolean = {
    if (!sid.satisfiesGeneralizedProgress)
      logger.warn(s"Discarding input because the (sub-)SID $sid does not satisfy progress.")
    sid.satisfiesGeneralizedProgress
  }

  private def getReachablePreds(sid: SID, curr: String, visited: Set[String] = Set.empty): Set[String] = {
    if (visited.contains(curr)) visited
    else {
      val withCurr = visited + curr
      val occurringPreds = sid(curr).rules.toSet[RuleBody].flatMap(_.body.predCalls).map(_.name)
      val reachableFromOccurring = occurringPreds.flatMap(getReachablePreds(sid, _, withCurr))
      // Note: Need to explicitly include withCurr in result because reachableFromOccurring may be empty!
      withCurr ++ reachableFromOccurring
    }
  }

  private def extractSidForCall(sid: SID, call: PredCall): Option[SID] = {
    val reachablePreds = getReachablePreds(sid, call.name)

    val res = sid.copy(startPred = call.name, preds = sid.preds.filter(pred => reachablePreds.contains(pred.head)))
    if (res.preds.nonEmpty) {
      Some(res)
    } else {
      logger.warn(s"Illegal specification: The SID doesn't contain any rules for ${call.name}")
      None
    }
  }

  private def extractSidForCalls(sid: SID, calls: Set[PredCall]): Option[SID] = {
    val predsByCall: Set[(String, Set[String])] = calls.map(_.name).map(p => (p, getReachablePreds(sid, p)))
    predsByCall find (_._2.isEmpty) match {
      case Some(value) =>
        logger.warn(s"Illegal specification: The SID doesn't contain any rules for ${value._1}")
        None
      case None =>
        // There are rules for all predicates => Filter SID accordingly & return
        val uniquePreds = predsByCall.flatMap(_._2)
        val res = sid.copy(startPred = "UNDEFINED", preds = sid.preds.filter(pred => uniquePreds.contains(pred.head)))
        Some(res)
    }
  }

  private def combineIntoSidForSide(defSid: SID, entailmentSid: SID): Option[SID] = {
    val entailmentPreds = entailmentSid.preds.map(_.head).toSet
    assert((defSid.preds.map(_.head).toSet intersect entailmentPreds).isEmpty)
    val allCalls = entailmentSid.preds.toSet[Predicate].flatMap(_.rules).flatMap(_.body.predCalls)
    val callsFromDefSid = allCalls filterNot(call => entailmentPreds.contains(call.name))
    val extractedDefSid = extractSidForCalls(defSid, callsFromDefSid)
    extractedDefSid.map {
      someSid => entailmentSid.copy(preds = entailmentSid.preds ++ someSid.preds)
    }
  }

  private def toSingleCall(sh: SymbolicHeap): Option[PredCall] = {
    if (sh.predCalls.size == 1 && sh.pointers.isEmpty && sh.pure.isEmpty) {
      sh.predCalls.headOption
    } else {
      None
    }
  }

}
