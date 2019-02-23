package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.ConstraintPropagation
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.seplog.sidtransformers.SidDirectionalityAnnotator.Directionality

import scala.annotation.tailrec
import scala.util.Try

// TODO: If we allow predicates that don't allocate pointers (like we now do in generalized progress), the root may not be unique!
// TODO: Even if we have progress, the focus need not be unique because multiple sinks can be free! Think about whether we want to explicitly allow or explicitly disallow this. (Probably not relevant for SL-COMP, but we'll see.)

class SidDirectionalityAnnotator(sid: SidLike, direction: Directionality) extends HarrshLogging {

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
      if rule.body.hasPointer || rule.body.predCalls.nonEmpty
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
        // namely if there is a root variable, but the predicate for the call with the root variable
        // is not yet in the currentAnnotation map
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
      val res = RichSid.fromSid(sid, roots, sinks)
      if (!res.isFocused) {
        val msg = s"$sid is unfocussed (rooted: ${roots.keys.mkString(", ")}, w/ sinks: ${sinks.keys.mkString(", ")})"
        throw PreprocessingException(msg)
      }
      res
    }
  }

  def computeRoots(sid: SidLike): Map[String, FreeVar] = {
    new SidDirectionalityAnnotator(sid, Root).run()
  }

  def computeSinks(sid: SidLike): Map[String, FreeVar] = {
    new SidDirectionalityAnnotator(sid, Sink).run()
  }

  sealed trait Directionality {
    val name: String
    def ptoArgsForDirection(pto: PointsTo): Set[Var]
  }
  case object Root extends Directionality {
    override def ptoArgsForDirection(pto: PointsTo): Set[Var] = Set(pto.from)

    override val name: String = "root"
  }
  case object Sink extends Directionality {
    override def ptoArgsForDirection(pto: PointsTo): Set[Var] = pto.to.toSet

    override val name: String = "sink"
  }

}
