package at.forsyte.harrsh.seplog.sidtransformers

import scala.util.Try
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConstraintPropagation
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._

import scala.annotation.tailrec

object AnnotateSidWithRootParams extends HarrshLogging {

  def apply(sid: Sid): Try[RichSid] = {
    Try {
      val rootedPreds = annotationFixedPoint(sid, Map.empty)
      RichSid(sid.startPred, sid.preds, sid.description, rootedPreds)
    }
  }

  @tailrec
  private def annotationFixedPoint(sid: Sid, currentAnnotation: Map[String, FreeVar]): Map[String, FreeVar] = {
    logger.debug("Starting annotation iteration from " + currentAnnotation)
    if (sid.preds.size == currentAnnotation.size) {
      currentAnnotation
    }
    else {
      val discoveredRoots = for {
        pred <- sid.preds
        if !currentAnnotation.isDefinedAt(pred.head)
        root <- uniqueRoot(currentAnnotation, pred, sid)
      } yield (pred.head, root)
      if (discoveredRoots.isEmpty) {
        val unrooted = sid.preds.map(_.head).toSet[String] -- currentAnnotation.keySet
        throw PreprocessingException(s"Cannot annotate predicates ${unrooted.mkString(",")} with roots in SID $sid")
      } else {
        val extendedAnnotation = currentAnnotation ++ discoveredRoots
        annotationFixedPoint(sid, extendedAnnotation)
      }
    }
  }

  private def uniqueRoot(currentAnnotation: Map[String, FreeVar], pred: Predicate, sid: Sid): Option[FreeVar] = {
    val allRoots = sourcesOfRules(currentAnnotation, pred, sid)
    logger.debug(s"Predicate $pred has roots: $allRoots")
    if (allRoots.isEmpty) throw PreprocessingException(s"Predicate $pred has no rule that allocates memory => Cannot add root")
    val intersection = allRoots.reduce(_ intersect _)

    if (intersection.isEmpty) {
      None
    } else {
      if (intersection.size > 1) {
        logger.debug(s"Will arbitrarily pick root among vars in $intersection")
      }
      intersection.headOption
    }
  }

  private def sourcesOfRules(currentAnnotation: Map[String, FreeVar], pred: Predicate, sid: Sid): Seq[Set[FreeVar]] = {
    for {
      rule <- pred.rules
      if rule.body.hasPointer || rule.body.predCalls.nonEmpty
    } yield sourcesOfSh(currentAnnotation, rule.body, sid)
  }

  private def sourcesOfSh(currentAnnotation: Map[String, FreeVar], body: SymbolicHeap, sid: Sid): Set[FreeVar] = {
    if (body.hasPointer) {
      body.pointers.toSet[PointsTo].flatMap {
        ptr => Var.freeNonNullVars(ConstraintPropagation.closeSetUnderEqualities(Set(ptr.from), body.pure))
      }
    } else {
      for {
        call <- body.predCalls.toSet[PredCall]
        root <- currentAnnotation.get(call.name).toSet[FreeVar]
        index = sid(call.name).params.indexOf(root)
        paramInstance = call.args(index)
        eq_to_root <- ConstraintPropagation.closeSetUnderEqualities(Set(paramInstance), body.pure)
        // Restrict to free vars here -- otherwise we might prematurely throw an exception,
        // namely if there is a root variable, but the predicate for the call with the root variable
        // is not yet in the currentAnnotation map
        if eq_to_root.isFree
      } yield eq_to_root.asInstanceOf[FreeVar]
    }
  }

}
