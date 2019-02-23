package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog._
import at.forsyte.harrsh.seplog.inductive.RichSid.{FocusedVar, RootFocus}
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec
import scala.util.Try

object SplitMultiPointerRules extends HarrshLogging {

  type FocusedPredicate = (Predicate, FocusedVar)

  /**
    * Split all multi-pointer rules in a rooted SID into single-pointer rules, thus establishing progress.
    *
    * Note: Rootedness is necessary if the split rules are non-linear -- otherwise it's unclear how to distribute the predicate calls
    * of the original rule among the new rules without losing connectivity.
    */
  def apply(sid: RichSid): Try[RichSid] = Try {
    val allPreds = sid.preds.flatMap(splitMultiPointerRules(sid, _))
    val (roots, sinks) = toRootSinkMaps(allPreds.map(pair => pair._1.head -> pair._2))
    sid.copy(preds = allPreds.map(_._1),
      description = s"PtoSplit(${sid.description}",
      roots = roots,
      sinks = sinks)
  }

  private def toRootSinkMaps(pairs: Seq[(String, FocusedVar)]): (Map[String,FreeVar], Map[String,FreeVar]) = {
    val (taggedRoots, taggedSinks) = pairs.partition(_._2.dir == RootFocus)
    def removeTag[A](taggedVal: (A,FocusedVar)): (A,FreeVar) = (taggedVal._1, taggedVal._2.fv)
    def toUntaggedMap[A,B,Tag](seq: Seq[(A,FocusedVar)]): Map[A,FreeVar] = seq.map(removeTag).toMap
    (toUntaggedMap(taggedRoots), toUntaggedMap(taggedSinks))
  }

  private case class Context(sid: RichSid, freeVarSeq: Seq[FreeVar], qvarNames: Seq[String], closure: Closure) {
    def toFreeVar(bv: BoundVar) = FreeVar(qvarNames(bv.index - 1))
  }

  private def splitMultiPointerRules(sid: RichSid, pred: Predicate): Seq[FocusedPredicate] = {
    val (twoOrMore, oneOrLess) = pred.rules.partition(_.body.pointers.size > 1)
    if (twoOrMore.nonEmpty) {

      val afterSplitting = twoOrMore.zipWithIndex.map {
        case (rule, index) => splitRule(sid, rule, PrefixOfUnfoldingAuxiliaryPreds + pred.head + index + "_")
      }
      val (transformedRules, newPreds) = afterSplitting.unzip
      val updatedPred = (Predicate(pred.head, oneOrLess ++ transformedRules), sid.focus(pred.head))
      logger.debug("Predicate after splitting: " + updatedPred)
      updatedPred +: newPreds.flatten
    } else {
      Seq((pred, sid.focus(pred.head)))
    }
  }

  private def splitRule(sid: RichSid, rule: RuleBody, predHeadPrefix: String): (RuleBody, Seq[FocusedPredicate]) = {
    if (!sid.isRooted) throw PreprocessingException(s"Cannot establish progress of ${rule.body} for unrooted SID")
    val pto = rule.body.pointers
    val (initial, other) = pto.partition(_.from.isFree)
    initial.size match {
      case 0 => throw PreprocessingException("Can't split un-rooted rule: " + rule)
      case 1 =>
        logger.debug(s"Will split rule ${rule.body}")
        val closure = Closure.fromSH(rule.body)
        val ctx = Context(sid, rule.body.freeVars, rule.qvarNames, closure)
        splitRuleOnPointer(ctx, initial.head, other, rule.body.predCalls, rule.body.pure, predHeadPrefix)
      case _ => throw PreprocessingException("Can't split rule with multiple root candidates: " + rule)
    }
  }

  case class SymbolicHeapWithVarInfo(sh: SymbolicHeap, originalFvSubSeq: Seq[FreeVar], originalBoundVarsTurnedFreeVars: Seq[(BoundVar,FreeVar)], remainingOriginalBoundVars: Set[BoundVar])

  case class PtoFraction(head: PointsTo, allPtos: Seq[PointsTo], calls: Seq[PredCall], allTargets: Set[Var]) {
    def add(pto: PointsTo, closure: Closure) = PtoFraction(head, allPtos :+ pto, calls, allTargets ++ targets(pto, closure))
    def add(call: PredCall): PtoFraction = copy(calls = calls :+ call)
    def canReach(v: Var): Boolean = allTargets.contains(v)

    def toSymbolicHeap(ctx: Context, boundVarsThatBecomeFree: Set[BoundVar]): SymbolicHeapWithVarInfo = {
      val atoms = AtomContainer(Seq.empty, allPtos, calls)
      val (bvarsToRename, otherBvars) = boundVars.partition(boundVarsThatBecomeFree)
      val bvarFvarPairs = bvarsToRename.toSeq map (v => (v,ctx.toFreeVar(v)))
      val newFreeVars = bvarFvarPairs.map(_._2)
      val freeVarSubseq = ctx.freeVarSeq.filter(originalFreeVars)
      val allFreeVars = freeVarSubseq ++ newFreeVars
      val renaming = Renaming.fromPairs(bvarFvarPairs)
      val withNewFreeVars = atoms.rename(renaming, avoidDoubleCapture = false)
      SymbolicHeapWithVarInfo(
        sh = SymbolicHeap(withNewFreeVars.closeGapsInBoundVars, allFreeVars),
        originalFvSubSeq = freeVarSubseq,
        originalBoundVarsTurnedFreeVars = bvarFvarPairs,
        remainingOriginalBoundVars = otherBvars)
    }

    lazy val originalFreeVars: Set[FreeVar] = Var.freeNonNullVars(allPtos.toSet[PointsTo].flatMap(_.getVars) ++ calls.flatMap(_.getVars))

    lazy val boundVars: Set[BoundVar] = Var.boundVars(allPtos.toSet[PointsTo].flatMap(_.getVars) ++ calls.flatMap(_.getVars))
  }

  object PtoFraction {
    def apply(pto: PointsTo, closure: Closure): PtoFraction = PtoFraction(pto, Seq(pto), Seq.empty, targets(pto, closure))
  }

  private def targets(pto: PointsTo, closure: Closure): Set[Var] = {
    pto.to.toSet[Var].flatMap(closure.getEquivalenceClass(_))
  }

  private def sharedBoundVars(pto: PointsTo, completedFractions: Seq[PtoFraction]): Set[BoundVar] = {
    val allBoundVars = Var.boundVars(pto.getVars).toSeq ++ completedFractions.flatMap(_.boundVars.toSeq)
    Combinators.counts(allBoundVars).filter(_._2 > 1).keySet
  }

  private def splitRuleOnPointer(ctx: Context, pto: PointsTo, otherPto: Seq[PointsTo], calls: Seq[PredCall], pure: Seq[PureAtom], prefix: String): (RuleBody, Seq[FocusedPredicate]) = {
    val allTargets = targets(pto, ctx.closure)
    // Split remainder into rooted components
    val (nextPtos, remainder) = otherPto.partition(pto => allTargets(pto.from))
    val ptoFractions = nextPtos map (PtoFraction(_, ctx.closure))
    // Sort to make deterministic
    val completedFractions = assignPtoToComponents(ctx, ptoFractions.toSet, remainder, calls).toSeq.sortBy(_.head.from)
    logger.debug("Connected components after dropping root pointer:\n" + completedFractions.mkString("\n"))

    val boundVarsThatBecomeFree = sharedBoundVars(pto, completedFractions)
    // Recursively introduce predicates for each fraction
    logger.debug(s"The bound variables $boundVarsThatBecomeFree are shared among multiple rules and will thus become free.")

    val newPreds: Seq[FocusedPredicateWithCallInfo] = completedFractions.zipWithIndex flatMap {
      case (ptoFrac, index) => makePredicates(ctx, ptoFrac, boundVarsThatBecomeFree, prefix + index)
    }

    val newPredCalls = newPreds map {
      pred => PredCall(pred.pred.head, pred.varsToPassToCall)
    }
    logger.debug("Will add calls to " + newPredCalls.mkString(", "))
    val newSh = SymbolicHeap(AtomContainer(pure, Seq(pto), newPredCalls).closeGapsInBoundVars, ctx.freeVarSeq)
    val boundVarsNotInNewSh = newPreds.toSet[FocusedPredicateWithCallInfo].flatMap(_.localBoundVars)
    logger.debug(s"Will discard bound vars $boundVarsNotInNewSh which are now bound vars in a new predicate")
    val boundVarIndicesNotInNewSh = boundVarsNotInNewSh.map(_.index)
    val newQVarNames = ctx.qvarNames.zipWithIndex.filterNot(pair => boundVarIndicesNotInNewSh(pair._2 + 1)).map(_._1)
    val newRuleBody = RuleBody(newQVarNames, newSh)
    logger.debug("New rule body: " + newRuleBody)
    val newFocusedPreds = newPreds map (p => (p.pred, p.focus))
    (newRuleBody, newFocusedPreds)
  }

  case class FocusedPredicateWithCallInfo(pred: Predicate, focus: FocusedVar, varsToPassToCall: Seq[Var], localBoundVars: Set[BoundVar])

  def makePredicates(ctx: Context, ptoFrac: PtoFraction, boundVarsThatBecomeFree: Set[BoundVar], prefix: String): Seq[FocusedPredicateWithCallInfo] = {
    if (ptoFrac.allPtos.size == 1) {
      val shWithVarInfo = ptoFrac.toSymbolicHeap(ctx, boundVarsThatBecomeFree)
      val root: FocusedVar = focusOf(ptoFrac, shWithVarInfo.originalBoundVarsTurnedFreeVars)
      val remainingBvIndices = shWithVarInfo.remainingOriginalBoundVars.map(_.index)
      val qvarNames = ctx.qvarNames.zipWithIndex.filter(pair => remainingBvIndices(pair._2 + 1)).map(_._1)
      assert(qvarNames.size == shWithVarInfo.sh.boundVars.size,
        s"New symbolic heap ${shWithVarInfo.sh} has bound vars ${shWithVarInfo.sh.boundVars.mkString(", ")}, but have different number of names ${qvarNames.mkString(",")}"
      )
      Seq(FocusedPredicateWithCallInfo(
        Predicate(prefix, Seq(RuleBody(qvarNames, shWithVarInfo.sh))),
        focus = root,
        varsToPassToCall = shWithVarInfo.originalFvSubSeq ++ shWithVarInfo.originalBoundVarsTurnedFreeVars.map(_._1),
        localBoundVars = shWithVarInfo.remainingOriginalBoundVars
      ))
    } else {
      // FIXME: Recurse, introduce multiple predicates as needed. (Apparently not urgent though, doesn't occur in the SL-COMP benchmarks.)
      throw PreprocessingException("No support for unfolding a non-progress rule multiple times")
    }
  }

  private def focusOf(ptoFrac: PtoFraction, originalBoundVarsTurnedFreeVars: Seq[(BoundVar, FreeVar)]): FocusedVar = {
    ptoFrac.head.from match {
      case fv: FreeVar => FocusedVar(fv, RootFocus)
      case bv: BoundVar => originalBoundVarsTurnedFreeVars.find(_._1 == bv) match {
        case None =>
          // FIXME: Compute sink in this case (or all cases)? Have to see whether this case occurs in SLCOMP benchmarks.
          throw PreprocessingException(s"Resulting predicate for $ptoFrac would be rooted in bound var $bv")
        case Some(pair) => FocusedVar(pair._2, RootFocus)
      }
      case NullConst => throw PreprocessingException("Null alloc")
    }
  }

  @tailrec
  private def assignPtoToComponents(ctx: Context, ptoFractions: Set[PtoFraction], remainder: Seq[PointsTo], calls: Seq[PredCall]): Set[PtoFraction] = {
    if (remainder.isEmpty) assignCallsToComponents(ctx, ptoFractions, calls)
    else
    {
      val (hd, tl) = (remainder.head, remainder.tail)
      ptoFractions.find(_.canReach(hd.from)) match {
        case None =>
          throw PreprocessingException(s"Cannot assign $hd to any of the components $ptoFractions")
        case Some(frac) =>
          val newFrac = frac.add(hd, ctx.closure)
          assignPtoToComponents(ctx, ptoFractions - frac + newFrac, tl, calls)
      }
    }
  }

  def assignCallsToComponents(ctx: Context, ptoFractions: Set[PtoFraction], calls: Seq[PredCall]): Set[PtoFraction] = {
    calls.foldLeft(ptoFractions) {
      case (fractions, call) =>
        ptoFractions.find(_.canReach(call.args(ctx.sid.rootParamIndex(call.name)))) match {
          case None =>
            throw PreprocessingException(s"Root parameter of $call not reachable from any component => Connectivity violated")
          case Some(frac) =>
            val newFrac = frac.add(call)
            fractions - frac + newFrac
        }
    }
  }

}
