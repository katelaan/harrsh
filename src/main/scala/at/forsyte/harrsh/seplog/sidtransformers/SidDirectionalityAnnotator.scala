package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConstraintPropagation
import at.forsyte.harrsh.seplog.inductive.RichSid.{FocusDirection, RootFocus, SinkFocus}
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._

import scala.annotation.tailrec
import scala.util.Try

// TODO: If we allow predicates that don't allocate pointers (like we now do in generalized progress), the root may not be unique!
// TODO: Even if we have progress, the focus need not be unique because multiple sinks can be free! Think about whether we want to explicitly allow or explicitly disallow this. (Probably not relevant for SL-COMP, but we'll see.)

class SidDirectionalityAnnotator(sid: SidLike, direction: FocusDirection) extends HarrshLogging {

  def run(): Map[String, FreeVar] = annotationFixedPoint(Map.empty)

  @tailrec
  private def annotationFixedPoint(currentAnnotation: Map[String, FreeVar]): Map[String, FreeVar] = {
    logger.debug("Starting annotation iteration from " + currentAnnotation)
    if (sid.preds.size == currentAnnotation.size) {
      currentAnnotation
    }
    else {
      val discoveredFocusVariables = for {
        pred <- sid.preds
        if !currentAnnotation.isDefinedAt(pred.head)
        root <- uniqueFocus(currentAnnotation, pred)
      } yield (pred.head, root)
      if (discoveredFocusVariables.isEmpty) {
        val unrooted = sid.preds.map(_.head).toSet[String] -- currentAnnotation.keySet
        currentAnnotation
      } else {
        val extendedAnnotation = currentAnnotation ++ discoveredFocusVariables
        annotationFixedPoint(extendedAnnotation)
      }
    }
  }

  private def uniqueFocus(currentAnnotation: Map[String, FreeVar], pred: Predicate): Option[FreeVar] = {
    val allFocusVars = focusOfRules(currentAnnotation, pred)
    logger.debug(s"Predicate $pred has roots: $allFocusVars")
    // FIXME: Do we want to deal with this special case? Seems uninteresting, but also seems stupid to fail on this kind of example. (Not relevant for SL-COMP, so not urgent.)
    if (allFocusVars.isEmpty) throw PreprocessingException(s"Predicate $pred has no rule that is nonempty => Cannot add ${direction.name}")
    val intersection = allFocusVars.reduce(_ intersect _)
    if (intersection.isEmpty) {
      None
    } else {
      if (intersection.size > 1) {
        logger.warn(s"Will arbitrarily pick ${direction.name} among vars in $intersection")
      }
      intersection.headOption
    }
  }

  private def focusOfRules(currentAnnotation: Map[String, FreeVar], pred: Predicate): Seq[Set[FreeVar]] = {
    for {
      rule <- pred.rules
      // A rule with pointers or calls is analyzed in the focus computation, unless...
      // If a rule of pred is "self-recursive", i.e., of the form pred(..) * ... pred(..), i.e., does not allocate memory
      // and exclusively calls itself, then it is ignored, since
      // (1) its focused must be defined by the other rules of the predicate
      // (2) if we don't ignore it, we will not discover the focus because of a chicken-egg problem: pred is not yet
      //     in the currentAnnotation and will thus never be added because the pred-rule which calls pred can never
      //     be analyzed.
      // FIXME: This is unsound for self-recursive rules that don't pass on free vars in root position!
      // TODO: This is still an incomplete focus computation, as e.g. the following predicate's focus can't be analyzed: p <= x1 -> x2; p <= p(x1 -> x2) * q(...)
      if rule.body.hasPointer || rule.body.predCalls.exists(_.name != pred.head)
    } yield focusOfSh(currentAnnotation, rule.body)
  }

  private def focusOfSh(currentAnnotation: Map[String, FreeVar], body: SymbolicHeap): Set[FreeVar] = {
    if (body.hasPointer) {
      body.pointers.toSet[PointsTo].flatMap {
        ptr => Var.freeNonNullVars(ConstraintPropagation.closeSetUnderEqualities(direction.ptoArgsForDirection(ptr), body.pure))
      }
    } else {
      for {
        call <- body.predCalls.toSet[PredCall]
        root <- currentAnnotation.get(call.name).toSet[FreeVar]
        index = sid(call.name).params.indexOf(root)
        paramInstance = call.args(index)
        eq_to_focus <- ConstraintPropagation.closeSetUnderEqualities(Set(paramInstance), body.pure)
        // Restrict to free vars here -- otherwise we might prematurely throw an exception,
        // namely if there is a root variable, some predicate that is called is already in the currentAnnotation map,
        // but the predicate of the call that allocates the root variable is not yet in the currentAnnotation map
        if eq_to_focus.isFree
      } yield eq_to_focus.asInstanceOf[FreeVar]
    }
  }

}

object SidDirectionalityAnnotator {

  def apply(sid: SidLike): Try[RichSid] = Try {
    val roots = computeRoots(sid)
    if (roots.size == sid.preds.size) {
      RichSid.fromSid(sid, roots)
    } else {
      val sinks = computeSinks(sid)
      RichSid.fromSid(sid, roots, sinks)
    }
  }

  def computeRoots(sid: SidLike): Map[String, FreeVar] = {
    new SidDirectionalityAnnotator(sid, RootFocus).run()
  }

  def computeSinks(sid: SidLike): Map[String, FreeVar] = {
    new SidDirectionalityAnnotator(sid, SinkFocus).run()
  }

}
