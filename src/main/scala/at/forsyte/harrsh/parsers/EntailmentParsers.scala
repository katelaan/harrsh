package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.entailment.{EntailmentInstance, EntailmentQuerySide, PredCalls}
import at.forsyte.harrsh.main.{HarrshLogging, MainIO}
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
      harrshEntailmentFormatToProcessedInstance(fileContent, computeSidsForEachSideOfEntailment)
    }
  }

  def parseHarrshEntailmentFormat(input: String): Option[EntailmentParseResult] = {
    for {
      pr <- DefaultEntailmentParser.run(input)
      if pr.lhs.predCalls forall (hasCorrectArity(_, pr.sid))
      if pr.rhs.predCalls forall (hasCorrectArity(_, pr.sid))
    } yield pr
  }

  def harrshEntailmentFormatToProcessedInstance(input: String, computeSeparateSidsForEachSide: Boolean): Option[EntailmentInstance] = {
    parseHarrshEntailmentFormat(input) flatMap (res => normalize(res, computeSeparateSidsForEachSide))
  }

  def normalize(parseRes: EntailmentParseResult, computeSeparateSidsForEachSide: Boolean) : Option[EntailmentInstance] = {
    generalizedProgressNormalform(computeSeparateSidsForEachSide)(parseRes) map logTransformationResult
  }

  private def logTransformationResult(instance: EntailmentInstance): EntailmentInstance = {
    logger.debug(s"Will perform entailment check ${instance.queryString} (instead of ${instance.originalQueryString} w.r.t. SIDs in progress normal form:")
    logger.debug(s"LHS SID:\n${instance.lhs.sid}")
    logger.debug(s"RHS SID:\n${instance.rhs.sid}")
    instance
  }

  private def generalizedProgressNormalform(computeSeparateSidsForEachSide: Boolean)(parseResult: EntailmentParseResult): Option[EntailmentInstance] = {
    for {
      rootedSid <- annotateSidWithRootParams(parseResult.sid)
      if satisfiesGeneralizedProgress(rootedSid)
      lhs = processEntailmentQuerySide(parseResult.lhs, rootedSid, computeSeparateSidsForEachSide, isLhs = true)
      rhs = processEntailmentQuerySide(parseResult.rhs, rootedSid, computeSeparateSidsForEachSide, isLhs = false)
    } yield EntailmentInstance(lhs, rhs, parseResult.entailmentHolds)
  }

  private def processEntailmentQuerySide(originalQuerySide: SymbolicHeap, rootedSid: SID, computeSeparateSidsForEachSide: Boolean, isLhs: Boolean): EntailmentQuerySide = {
    val sideSid = if (computeSeparateSidsForEachSide) extractSidForCalls(rootedSid, originalQuerySide.predCalls.toSet) else rootedSid
    val (sccSid, lhsCalls) = splitQueryIntoSccs(originalQuerySide, sideSid, isLhs)
    EntailmentQuerySide(sccSid, lhsCalls, originalQuerySide)
  }

  private def splitQueryIntoSccs(querySide: SymbolicHeap, sid: SID, isLhs: Boolean): (SID, PredCalls) = {
    logger.debug(s"Will transform $querySide into one call per SCC, starting from SID\n$sid")
    ToSymbolicHeapOverBtwSid(querySide, if (isLhs) PrefixOfLhsAuxiliaryPreds else PrefixOfRhsAuxiliaryPreds, sid)
  }

  private def hasCorrectArity(call: PredCall, sid: SID) = {
    val res = call.args.length == sid(call.name).arity
    if (!res) {
      logger.error(s"Invalid input: Query contains call $call, but predicate ${call.name} has arity ${sid(call.name).arity}")
    }
    res
  }

  object annotateSidWithRootParams {

    def apply(sid: SID): Option[SID] = {
      Try {
        val rootedPreds = sid.preds map annotatePredWithRoot
        sid.copy(preds = rootedPreds)
      } match {
        case Failure(exception) =>
          logger.error(s"Can't annotate SID with root parameters: ${exception.getMessage}")
          None
        case Success(annotatedSID) => Some(annotatedSID)
      }
    }

    private def annotatePredWithRoot(pred: Predicate): Predicate = {
      pred.rules.find(!_.satisfiesGeneralizedProgress(rootOfPred = None)).map {
        rule => throw new IllegalArgumentException(s"SID contains a rule that violates progress: $rule")
      }

      // TODO: The following fails for SIDs that satisfy generalized progress but not strict progress (rootVars will then be empty). This could be fixed if we wanted to add support for generalized progress in the inputs (as opposed to only for the generated top-level predicates)
      val rootVars = sourcesOfHeadPtrs(pred)
      if (rootVars.size == 1) pred.copy(rootParam = rootVars.headOption.map(_.asInstanceOf[FreeVar]))
      else throw new IllegalArgumentException(s"No unique root parameter in predicate $pred; roots: $rootVars")
    }

    private def sourcesOfHeadPtrs(pred: Predicate): Set[Var] = {
      pred.rules.flatMap(_.body.pointers.headOption.map(_.from)).toSet
    }

  }

  private def satisfiesGeneralizedProgress(sid: SID): Boolean = {
    if (!sid.satisfiesGeneralizedProgress)
      logger.error(s"Discarding input because the SID $sid does not satisfy progress.")
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

  def extractSidForCalls(sid: SID, calls: Set[PredCall]): SID = {
    val predsByCall: Set[(String, Set[String])] = calls.map(_.name).map(p => (p, getReachablePreds(sid, p)))
    predsByCall find (_._2.isEmpty) match {
      case Some(value) =>
        throw new IllegalArgumentException(s"Illegal specification: The SID doesn't contain any rules for ${value._1}")
      case None =>
        // There are rules for all predicates => Filter SID accordingly & return
        val uniquePreds = predsByCall.flatMap(_._2)
        sid.copy(startPred = "UNDEFINED", preds = sid.preds.filter(pred => uniquePreds.contains(pred.head)))
    }
  }

}
