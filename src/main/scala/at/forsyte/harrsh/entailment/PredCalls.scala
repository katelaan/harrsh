package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{EmptyPredicates, PredCall, PureAtom, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

case class PredCalls(calls: Seq[PredCall]) extends HarrshLogging {

  lazy val size = calls.size
  lazy val names = calls map (_.name)
  lazy val orderedCalls = calls.sortBy(_.name)
  lazy val paramsByName: Map[String, Seq[Seq[Var]]] = calls.groupBy(_.name).map{
    case (name, matches) => (name, matches.map(_.args))
  }

  override def toString: String = calls.mkString(" * ")

  def toSymbolicHeap = SymbolicHeap(calls:_*)

  def isImpliedBy(lhs: Set[ContextPredCall], lhsEnsuredPure: Set[PureAtom], predsWithEmptyModels: EmptyPredicates): Boolean = {
    val orderedLhs = PredCalls.sorted(lhs)
    val (rhsCallsPresentInLhs, callsMissingInLhs, extraPredsInLhs) = PredCalls.compareOrderedPredSeqs(orderedLhs, orderedCalls)
    if (extraPredsInLhs.nonEmpty) {
      logger.debug(s"$lhs does not imply $this, because it contains extra predicate call(s) ${extraPredsInLhs.mkString(" * ")}")
      false
    } else {
      val (possiblyEmptyPreds, nonemptyPreds) = callsMissingInLhs.partition(canBeEmpty(_, lhsEnsuredPure, predsWithEmptyModels))
      if (nonemptyPreds.isEmpty) {
        PredCalls.matchable(orderedLhs, rhsCallsPresentInLhs, paramsByName)
      }
      else {
        logger.debug(s"The predicate(s) ${nonemptyPreds.mkString(", ")} can't be empty, but are missing on the LHS.")
        false
      }
    }
  }

  private def canBeEmpty(rhsPredCall: PredCall, lhsEnsuredPure: Set[PureAtom], predsWithEmptyModels: EmptyPredicates): Boolean = {
    if (predsWithEmptyModels.hasEmptyModels(rhsPredCall.name)) {
      logger.debug(s"Check whether $rhsPredCall can be interpreted by empty model under the pure constraints $lhsEnsuredPure")
      ???
    } else {
      logger.debug(s"RHS $rhsPredCall can't have empty models, but does not occur on the LHS => LHS does not imply RHS")
      false
    }
  }

}

object PredCalls extends HarrshLogging {

  def sorted(calls: Iterable[ContextPredCall]): Seq[ContextPredCall] = calls.toSeq.sortBy(_.pred.head)

  def matchable(lhsCalls: Seq[ContextPredCall], rhs: Seq[PredCall], rhsParamsByName: Map[String, Seq[Seq[Var]]]): Boolean = {
    // TODO: Cache this in the decomposition?
    val otherByName: Map[String, Seq[Substitution]] = lhsCalls.groupBy(_.pred.head).map{
      case (name, matches) => (name, matches.map(_.subst))
    }

    val res = PredCalls.callsMatchableWithoutRenaming(rhsParamsByName, otherByName)
    logger.debug(s"LHS $lhsCalls matchable against (partial) RHS ${rhs.mkString(" * ")}? ===> $res")
    res
  }

  def compareOrderedPredSeqs(lhsPreds: Seq[ContextPredCall], rhsPreds: Seq[PredCall]): (Seq[PredCall], Seq[PredCall], Seq[ContextPredCall]) = {
    if (lhsPreds.isEmpty) (Seq.empty, rhsPreds, Seq.empty)
    else if (rhsPreds.isEmpty) (Seq.empty, Seq.empty, lhsPreds)
    else {
      val (lhsHd, rhsHd) = (lhsPreds.head.pred.head, rhsPreds.head.name)
      if (lhsHd == rhsHd) {
        val (present, missing, extra) = compareOrderedPredSeqs(lhsPreds.tail, rhsPreds.tail)
        (rhsPreds.head +: present, missing, extra)
      }
      else if (lhsHd > rhsHd) {
        // The RHS head predicate does not occur on the left
        val (present, missing, extra) = compareOrderedPredSeqs(lhsPreds, rhsPreds.tail)
        (present, rhsPreds.head +: missing, extra)
      } else {
        // The LHD head predicate does not occur on the right
        assert(lhsHd < rhsHd)
        val (present, missing, extra) = compareOrderedPredSeqs(lhsPreds.tail, rhsPreds)
        (present, missing, lhsPreds.head +: extra)
      }
    }
  }

  def callsMatchableWithoutRenaming(lhs: Map[String, Seq[Seq[Var]]], rhs: Map[String, Seq[Substitution]]): Boolean = {
    assert(lhs.keySet == rhs.keySet)
    lhs.keys.forall(pred => callargsAndSubstsMatch(lhs(pred), rhs(pred)))
  }

  private def callargsAndSubstsMatch(lhsArgs: Seq[Seq[Var]], rhsSubsts: Seq[Substitution]): Boolean = {
    Combinators.permutations(lhsArgs).exists(argsSeqImplySubstSeq(_, rhsSubsts))
  }

  private def argsSeqImplySubstSeq(lhsLinearization: Seq[Seq[Var]], rhsSubsts: Seq[Substitution]): Boolean = {
    (lhsLinearization, rhsSubsts).zipped.forall{
      case (args,subst) => argsImplySubst(args, subst)
    }
  }

  private def argsImplySubst(args: Seq[Var], subst: Substitution): Boolean = {
    (args, subst.toSeq).zipped.forall{
      case (arg, substVal) => substVal.contains(arg)
    }
  }

}