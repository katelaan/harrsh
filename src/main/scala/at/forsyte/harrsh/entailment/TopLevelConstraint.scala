package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.PureEntailment
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{EmptyPredicates, PredCall, PureAtom, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

case class TopLevelConstraint(calls: Seq[PredCall], pure: Seq[PureAtom]) extends HarrshLogging {

  lazy val size: NodeId = calls.size
  lazy val names: Seq[String] = calls map (_.name)
  lazy val orderedCalls: Seq[PredCall] = calls.sortBy(_.name)

  override def toString: String = {
    val callsStr = calls.mkString(" * ")
    val pureStr = if (pure.nonEmpty) pure.mkString("{", ", ", "}") else ""
    Seq(callsStr, pureStr).filter(_.nonEmpty).mkString(" : ")
  }

  def isQuantifierFree: Boolean = toSymbolicHeap.boundVars.isEmpty

  def toSymbolicHeap = SymbolicHeap((calls ++ pure):_*)

  def isImpliedBy(lhs: Set[ContextPredCall], lhsEnsuredPure: Set[PureAtom], predsWithEmptyModels: EmptyPredicates): Boolean = {
    pureEntailmentHolds(lhsEnsuredPure) && heapEntailmentHolds(lhs, lhsEnsuredPure, predsWithEmptyModels)
  }

  private def pureEntailmentHolds(lhsEnsuredAtoms: Set[PureAtom]): Boolean = {
    val res = pure.isEmpty || PureEntailment.check(lhsEnsuredAtoms.toSeq, pure)
    if (!res) logger.debug(s"LHS pure constraints $lhsEnsuredAtoms don't imply RHS pure constraints $pure => Implication does not hold")
    res
  }

  private def heapEntailmentHolds(lhs: Set[ContextPredCall], lhsEnsuredPure: Set[PureAtom], predsWithEmptyModels: EmptyPredicates): Boolean = {
    val orderedLhs = TopLevelConstraint.sorted(lhs)
    val (rhsCallsPresentInLhs, callsMissingInLhs, extraPredsInLhs) = TopLevelConstraint.compareOrderedPredSeqs(orderedLhs, orderedCalls)
    if (extraPredsInLhs.nonEmpty) {
      logger.debug(s"$lhs does not imply $this, because it contains extra predicate call(s) ${extraPredsInLhs.mkString(" * ")}")
      false
    } else {
      val (possiblyEmptyPreds, nonemptyPreds) = callsMissingInLhs.partition(canBeEmpty(_, lhsEnsuredPure, predsWithEmptyModels))
      if (nonemptyPreds.isEmpty) {
        logger.debug(s"Will try to match $orderedLhs against ${rhsCallsPresentInLhs.mkString(" * ")}")
        val res = TopLevelConstraint.matchable(orderedLhs, rhsCallsPresentInLhs)
        logger.debug(s"LHS $lhs matchable against (partial) RHS ${rhsCallsPresentInLhs.mkString(" * ")}? ===> $res")
        res
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
      val options = predsWithEmptyModels(rhsPredCall)
      // TODO This only works if the pure constraints are closed. Make sure this is always the case!
      options find (_ subsetOf lhsEnsuredPure) match {
        case None =>
          logger.debug(s"LHS pure constraints $lhsEnsuredPure don't imply ${options.mkString(" or ")}")
          false
        case Some(option) =>
          logger.debug(s"$lhsEnsuredPure implies $option => Can use empty interpretation of $rhsPredCall")
          true
      }
    } else {
      logger.debug(s"RHS $rhsPredCall can't have empty models, but does not occur on the LHS => LHS does not imply RHS")
      false
    }
  }

}

object TopLevelConstraint extends HarrshLogging {

  def sorted(calls: Iterable[ContextPredCall]): Seq[ContextPredCall] = calls.toSeq.sortBy(_.pred.head)

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

  def matchable(lhsCalls: Seq[ContextPredCall], rhsCalls: Seq[PredCall]): Boolean = {
    assert(lhsCalls.size == rhsCalls.size,
      s"Trying to match call sequences of different length: $lhsCalls against $rhsCalls")

    // TODO: Cache this in the decomposition?
    val lhsSubstByPredName: Map[String, Seq[Substitution]] = lhsCalls.groupBy(_.pred.head).map{
      case (name, matches) => (name, matches.map(_.subst))
    }

    val rhsParamsByPredName: Map[String, Seq[Seq[Var]]] = rhsCalls.groupBy(_.name).map{
      case (name, matches) => (name, matches.map(_.args))
    }

    callsMatchableWithoutRenaming(rhsParamsByPredName, lhsSubstByPredName)
  }

  def callsMatchableWithoutRenaming(lhs: Map[String, Seq[Seq[Var]]], rhs: Map[String, Seq[Substitution]]): Boolean = {
    assert(lhs.keySet == rhs.keySet,
      s"Trying to match $lhs against $rhs, but only one of the maps contains ${lhs.keySet diff rhs.keySet}")
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