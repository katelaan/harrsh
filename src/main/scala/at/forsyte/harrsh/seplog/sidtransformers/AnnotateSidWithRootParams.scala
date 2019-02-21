package at.forsyte.harrsh.seplog.sidtransformers

import scala.util.{Failure, Success, Try}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._

object AnnotateSidWithRootParams extends HarrshLogging {

  def apply(sid: Sid): Option[RichSid] = {
    Try {
      val rootedPreds = sid.preds map (pred => (pred.head, rootOfPred(pred)))
      RichSid(sid.startPred, sid.preds, sid.description, rootedPreds.toMap)
    } match {
      case Failure(exception) =>
        logger.error(s"Can't annotate SID with root parameters: ${exception.getMessage}")
        None
      case Success(annotatedSID) => Some(annotatedSID)
    }
  }

  private def rootOfPred(pred: Predicate): FreeVar = {
    pred.rules.find(!_.satisfiesGeneralizedProgress(rootOfPred = None)).map {
      rule => throw new IllegalArgumentException(s"SID contains a rule that violates progress: $rule")
    }

    // TODO: The following fails for SIDs that satisfy generalized progress but not strict progress (rootVars will then be empty). This could be fixed if we wanted to add support for generalized progress in the inputs (as opposed to only for the generated top-level predicates)
    val rootVars = sourcesOfHeadPtrs(pred)
    if (rootVars.size == 1) rootVars.head.asInstanceOf[FreeVar]
    else throw new IllegalArgumentException(s"No unique root parameter in predicate $pred; roots: $rootVars")
  }

  private def sourcesOfHeadPtrs(pred: Predicate): Set[Var] = {
    pred.rules.flatMap(_.body.pointers.headOption.map(_.from)).toSet
  }

}
