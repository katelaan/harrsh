package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{EmptyPredicates, PredCall, PureAtom, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

case class TopLevelConstraint(calls: Seq[PredCall], pure: Seq[PureAtom]) extends HarrshLogging {

  lazy val size: NodeId = calls.size

  override def toString: String = toSymbolicHeap.toString

  def isQuantifierFree: Boolean = boundVars.isEmpty

  lazy val nonNullVars: Seq[Var] = toSymbolicHeap.freeVars ++ toSymbolicHeap.boundVars
  lazy val boundVars: Set[Var] = toSymbolicHeap.boundVars.toSet

  lazy val toSymbolicHeap = SymbolicHeap((calls ++ pure):_*)

  def isImpliedBy(lhs: Set[ContextPredCall], lhsConstraints: VarConstraints, predsWithEmptyModels: EmptyPredicates): Boolean = {
    pureEntailmentHolds(lhsConstraints) && heapEntailmentHolds(lhs, lhsConstraints, predsWithEmptyModels)
  }

  private def pureEntailmentHolds(lhsConstraints: VarConstraints): Boolean = {
    val res = lhsConstraints.impliesWithoutSpeculation(pure)
    if (!res) logger.debug(s"LHS constraints $lhsConstraints don't imply RHS pure constraints $pure => Implication does not hold")
    res
  }

  private def heapEntailmentHolds(lhs: Set[ContextPredCall], lhsConstraints: VarConstraints, predsWithEmptyModels: EmptyPredicates): Boolean = {
    val lhsByPred: Map[String, Set[ContextPredCall]] = lhs.groupBy(_.pred.head)
    val rhsByPred = calls.groupBy(_.name)
    val occurringPreds = lhsByPred.keySet ++ rhsByPred.keySet
    occurringPreds forall {
      pred => callsWithSameHeadAreMatchable(lhsByPred.getOrElse(pred, Set.empty),
        rhsByPred.getOrElse(pred, Seq.empty),
        lhsConstraints,
        predsWithEmptyModels)
    }
  }

  private def callsWithSameHeadAreMatchable(lhsCalls: Set[ContextPredCall], rhsCalls: Seq[PredCall], lhsConstraints: VarConstraints, predsWithEmptyModels: EmptyPredicates): Boolean = {
    if (lhsCalls.size > rhsCalls.size) {
      logger.debug(s"LHS $lhsCalls do not imply RHS $rhsCalls, because LHS contains additional predicate call(s)")
      false
    } else {
      canMatchOneByOne(lhsCalls, rhsCalls, lhsConstraints, predsWithEmptyModels)
    }
  }

  private def canReplaceMissingCallsByEmpty(rhsCalls: Seq[PredCall], lhsConstraints: VarConstraints, predsWithEmptyModels: EmptyPredicates): Boolean = {
    rhsCalls forall (canBeEmpty(_, lhsConstraints, predsWithEmptyModels))
  }

  @tailrec
  private def canMatchOneByOne(lhsCalls: Set[ContextPredCall], rhsCalls: Seq[PredCall], lhsConstraints: VarConstraints, predsWithEmptyModels: EmptyPredicates): Boolean = {
    if (lhsCalls.isEmpty) {
      if (rhsCalls.isEmpty) {
        logger.debug("Matching succeeded. Entailment holds.")
        true
      } else {
        logger.debug(s"All LHS calls have been matched. Entailment holds if remainin RHS calls $rhsCalls can be empty under constraints $lhsConstraints")
        canReplaceMissingCallsByEmpty(rhsCalls, lhsConstraints, predsWithEmptyModels)
      }
    } else if (rhsCalls.isEmpty) {
      logger.debug(s"LHS calls $lhsCalls could not be matched => Entailment does not hold")
      false
    } else {
      val (hd, tl) = (rhsCalls.head, rhsCalls.tail)
      lhsCalls.find(call => TopLevelConstraint.argsImplySubst(hd.args, call.subst)) match {
        case None =>
          if (canBeEmpty(hd, lhsConstraints, predsWithEmptyModels)) {
            logger.debug(s"Can't match RHS call $hd against any of the unmatched LHS calls $lhsCalls, but it can be empty => Continue matching")
            canMatchOneByOne(lhsCalls, tl, lhsConstraints, predsWithEmptyModels)
          } else {
            logger.debug(s"Can't match RHS call $hd against any of the unmatched LHS calls $lhsCalls and it can't be empty => Entailment does not hold")
            false
          }
        case Some(lhsCall) =>
          logger.debug(s"Matched $lhsCall |= $hd. Will continue matching.")
          canMatchOneByOne(lhsCalls - lhsCall, tl, lhsConstraints, predsWithEmptyModels)
      }
    }
  }

  private def canBeEmpty(rhsPredCall: PredCall, lhsConstraints: VarConstraints, predsWithEmptyModels: EmptyPredicates): Boolean = {
    if (predsWithEmptyModels.hasEmptyModels(rhsPredCall.name)) {
      logger.debug(s"Check whether $rhsPredCall can be interpreted by empty model under the constraints $lhsConstraints")
      val options = predsWithEmptyModels(rhsPredCall)
      options find (pure => lhsConstraints.impliesWithoutSpeculation(pure)) match {
        case None =>
          logger.debug(s"LHS constraints $lhsConstraints don't imply ${options.mkString(" or ")}")
          false
        case Some(option) =>
          logger.debug(s"$lhsConstraints implies $option => Can use empty interpretation of $rhsPredCall")
          true
      }
    } else {
      logger.debug(s"RHS $rhsPredCall can't have empty models, but does not occur on the LHS => LHS does not imply RHS")
      false
    }
  }

}

object TopLevelConstraint extends HarrshLogging {

  def fromSH(sh: SymbolicHeap) = TopLevelConstraint(sh.predCalls, sh.pure)

  def argsImplySubst(args: Seq[Var], subst: Substitution): Boolean = {
    (args, subst.toSeq).zipped.forall{
      case (arg, substVal) => substVal.contains(arg)
    }
  }

}