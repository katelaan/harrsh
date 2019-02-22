package at.forsyte.harrsh.seplog.sidtransformers

import scala.util.{Failure, Success, Try}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConstraintPropagation
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
    // TODO: The following fails for SIDs that satisfy generalized progress but not strict progress (rootVars will then be empty). This could be fixed if we wanted to add support for generalized progress in the inputs (as opposed to only for the generated top-level predicates)
    pred.rules.find(!_.satisfiesGeneralizedProgress(rootOfPred = None)).map {
      rule => throw PreprocessingException(s"SID contains a rule that violates progress: $rule")
    }
    uniqueRoot(pred)
  }

  private def uniqueRoot(pred: Predicate): FreeVar = {
    val allRoots = sourcesOfHeadPtrs(pred)
    val intersection = allRoots.reduce(_ intersect _)
    val root = intersection.size match {
      case 0 =>
        throw PreprocessingException(s"No unique root parameter in predicate $pred; roots by rule: ${allRoots.mkString(",")})")
      case 1 => intersection.head
      case _ =>
        logger.debug(s"Will arbitrarily pick root among free vars in $intersection")
        intersection.filter(_.isFree).head
    }
    root match {
      case v:FreeVar => v
      case v =>
        throw PreprocessingException(s"Can't use non-free var $v as root.")
    }
  }

  private def sourcesOfHeadPtrs(pred: Predicate): Seq[Set[Var]] = {
    for {
      rule <- pred.rules
      ptr <- rule.body.pointers.headOption
    } yield ConstraintPropagation.closeSetUnderEqualities(Set(ptr.from), rule.body.pure)
  }

}
