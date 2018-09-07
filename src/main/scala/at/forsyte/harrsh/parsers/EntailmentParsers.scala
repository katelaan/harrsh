package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.entailment.EntailmentChecker.EntailmentInstance
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.parsers.buildingblocks.{AsciiAtoms, EmptyQuantifierPrefix}
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive._

import scala.util.{Failure, Success, Try}

object EntailmentParsers extends HarrshLogging {

  val DefaultEntailmentParser = new EntailmentParser with HarrshSIDParser with AsciiAtoms with EmptyQuantifierPrefix

  def parse(input: String): Option[EntailmentInstance] = {
    val coInstance = Try { DefaultEntailmentParser.run(input).flatMap(transformToCallOnlyInstance) }.toOption.flatten
    if (coInstance.isEmpty) {
      DefaultEntailmentParser.run(input).flatMap(transformToInstance)
    } else {
      coInstance
    }
    //DefaultEntailmentParser.run(input).flatMap(transformToInstance)
  }

  def parseCallOnlyInstance(input: String): Option[EntailmentInstance] = {
    DefaultEntailmentParser.run(input).flatMap(transformToCallOnlyInstance)
  }

  // TODO: Get rid of this restricted transformation once the more general one is working correctly
  private def transformToCallOnlyInstance(parseResult: EntailmentParser.EntailmentParseResult): Option[EntailmentInstance] = {
    for {
      lhsCall <- toSingleCall(parseResult.lhs)
      if correctArity(lhsCall, parseResult.sid)
      _ = if (lhsCall.args != parseResult.sid(lhsCall.name).params) throw new NotImplementedError(s"Currently no support for parameter reordering on the left-hand side of the query; try ${parseResult.sid(lhsCall.name).defaultCall} instead of ${lhsCall}")
      rhsCall <- toSingleCall(parseResult.rhs)
      if correctArity(rhsCall, parseResult.sid)
      _ = if (!rhsCall.args.forall(_.isFreeNonNull)) throw new NotImplementedError(s"Can't check entailment against $rhsCall: Currently no support for using bound variables and/or null in the query")
      rootedSID <- makeRooted(parseResult.sid)
      lhsSid <- extractSidForCall(rootedSID, lhsCall)
      rhsSid <- extractSidForCall(rootedSID, rhsCall)
      if satisfiesProgress(lhsSid) && satisfiesProgress(rhsSid)
    } yield EntailmentInstance(lhsSid, lhsCall, rhsSid, rhsCall, parseResult.entailmentHolds)
  }

  private def transformToInstance(parseResult: EntailmentParser.EntailmentParseResult): Option[EntailmentInstance] = {
    logger.debug(s"Will establish progress normal form for parse result ${parseResult.lhs} |= ${parseResult.rhs} for SID\n${parseResult.sid}")
    for {
      rootedSID <- makeRooted(parseResult.sid)
      if satisfiesProgress(rootedSID)
      lhsPreds = SIDUtils.shToProgressSid(parseResult.lhs, "lhs")
      rhsPreds = SIDUtils.shToProgressSid(parseResult.rhs, "rhs")
      // TODO: Don't introduce extra predicates when the top-level query is already a call (at least for the RHS, where we already support parameter reordering)
      lhsSid <- extractSidForSide(rootedSID, lhsPreds)
      rhsSid <- extractSidForSide(rootedSID, rhsPreds)
      rhsCall = rhsSid.callToStartPred.predCalls.head
      lhsCall = lhsSid.callToStartPred.predCalls.head
      _ = {
        logger.debug(s"Will perform entailment check $lhsCall |= $rhsCall w.r.t. SIDs in progress normal form:")
        logger.debug(s"LHS SID:\n$lhsSid")
        logger.debug(s"RHS SID:\n$rhsSid")
      }
    } yield EntailmentInstance(lhsSid, lhsCall, rhsSid, rhsCall, parseResult.entailmentHolds)
  }

  private def correctArity(call: PredCall, sid: SID) = {
    val res = call.args.length == sid(call.name).arity
    if (!res) {
      logger.warn(s"Invalid input: Query contains call $call, but predicate ${call.name} has arity ${sid(call.name).arity}")
    }
    res
  }

  private def makeRooted(sid: SID): Option[SID] = {
    Try {
      def makePredRooted(pred: Predicate): Predicate = {
        pred.rules.find(_.body.pointers.isEmpty).map {
          rule => throw new IllegalArgumentException(s"SID contains a rule that violates progress: $rule")
        }

        val rootVars = pred.rules.map(_.body.pointers.head.from).toSet
        if (rootVars.size == 1) pred.copy(rootParam = rootVars.headOption.map(_.asInstanceOf[FreeVar]))
        else throw new IllegalArgumentException(s"No unique root parameter in predicate $pred; roots: $rootVars")
      }

      val rootedPreds: Seq[Predicate] = for (pred <- sid.preds) yield makePredRooted(pred)
      sid.copy(preds = rootedPreds)
    } match {
      case Failure(exception) =>
        logger.warn(s"Can't annotate SID with root parameters: ${exception.getMessage}")
        None
      case Success(annotatedSID) => Some(annotatedSID)
    }
  }

  private def satisfiesProgress(sid: SID): Boolean = {
    if (!sid.satisfiesProgress)
      logger.warn(s"Discarding input because the (sub-)SID $sid does not satisfy progress.")
    sid.satisfiesProgress
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

  private def extractSidForSide(defSid: SID, entailmentSid: SID): Option[SID] = {
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
      logger.warn(s"Currently Harrsh only supports single calls on both sides of the entailment")
      None
    }
  }

}
