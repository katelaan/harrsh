package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.parsers.buildingblocks.{AsciiAtoms, EmptyQuantifierPrefix}
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive._

import scala.util.{Failure, Success, Try}

object EntailmentParsers extends HarrshLogging {

  val DefaultEntailmentParser = new EntailmentParser with HarrshSIDParser with AsciiAtoms with EmptyQuantifierPrefix

  case class EntailmentInstance(lhsSid: SID, lhsCall: PredCall, rhsSid: SID, rhsCall: PredCall, entailmentHolds: Option[Boolean])

  def parse(input: String): Option[EntailmentInstance] = {
    DefaultEntailmentParser.run(input).flatMap(transformToInstance)
  }

  private def transformToInstance(parseResult: EntailmentParser.EntailmentParseResult): Option[EntailmentInstance] = {
    for {
      lhsCall <- toSingleCall(parseResult.lhs)
      rhsCall <- toSingleCall(parseResult.rhs)
      rootedSID <- makeRooted(parseResult.sid)
      lhsSid <- extractSidForCall(rootedSID, lhsCall)
      rhsSid <- extractSidForCall(rootedSID, rhsCall)
      if satisfiesProgress(lhsSid) && satisfiesProgress(rhsSid)
    } yield EntailmentInstance(lhsSid, lhsCall, rhsSid, rhsCall, parseResult.entailmentHolds)
  }

  private def makeRooted(sid: SID): Option[SID] = {
    Try {
      def makePredRooted(pred: Predicate): Predicate = {
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

  private def extractSidForCall(sid: SID, call: PredCall): Option[SID] = {
    def getReachablePreds(curr: String, visited: Set[String] = Set.empty): Set[String] = {
      if (visited.contains(curr)) visited
      else {
        val withCurr = visited + curr
        val occurringPreds = sid(curr).rules.toSet[RuleBody].flatMap(_.body.predCalls).map(_.name)
        val reachableFromOccurring = occurringPreds.flatMap(getReachablePreds(_, withCurr))
        // Note: Need to explicitly include withCurr in result because reachableFromOccurring may be empty!
        withCurr ++ reachableFromOccurring
      }
    }

    val reachablePreds = getReachablePreds(call.name)

    val res = sid.copy(startPred = call.name, preds = sid.preds.filter(pred => reachablePreds.contains(pred.head)))
    if (res.preds.nonEmpty) {
      Some(res)
    } else {
      logger.warn(s"Illegal specification: The SID doesn't contain any rules for ${call.name}")
      None
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
