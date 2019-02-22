package at.forsyte.harrsh.seplog.sidtransformers

import scala.util.Try
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConstraintPropagation
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._

object AnnotateSidWithRootParams extends HarrshLogging {

  def apply(sid: Sid): Try[RichSid] = {
    Try {
      val rootedPreds = sid.preds map (pred => (pred.head, uniqueRoot(pred)))
      RichSid(sid.startPred, sid.preds, sid.description, rootedPreds.toMap)
    }
  }

  private def uniqueRoot(pred: Predicate): FreeVar = {
    val allRoots = sourcesOfPtrs(pred)
    logger.debug(s"Predicate $pred has roots: $allRoots")
    // FIXME: If we know the dependencies / call graph for the predicates, we can avoid this by computing the root of the called predicates first
    if (allRoots.isEmpty) throw PreprocessingException(s"Predicate $pred has no rule that allocates memory => Cannot add root")
    val intersection = allRoots.reduce(_ intersect _)
    val root = intersection.size match {
      case 0 =>
        throw PreprocessingException(s"Root parameter annotation: No unique root parameter in predicate $pred; roots by rule: ${allRoots.mkString(",")})")
      case 1 => intersection.head
      case _ =>
        logger.debug(s"Will arbitrarily pick root among free vars in $intersection")
        intersection.filter(_.isFree).head
    }
    root match {
      case v:FreeVar => v
      case v =>
        throw PreprocessingException(s"Root parameter annotation: Can't use non-free var $v as root.")
    }
  }

  private def sourcesOfPtrs(pred: Predicate): Seq[Set[Var]] = {
    for {
      rule <- pred.rules
      if rule.body.hasPointer
    } yield rule.body.pointers.toSet[PointsTo].flatMap {
      ptr => ConstraintPropagation.closeSetUnderEqualities(Set(ptr.from), rule.body.pure)
    }
  }

}
