package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.{Renaming, Var}
import at.forsyte.harrsh.util.Combinators

class EmptyPredicates(val underlying: Map[String, Set[Set[PureAtom]]]) {

  lazy val nonEmpty = underlying.nonEmpty

  def hasEmptyModels(pred: String): Boolean = underlying.isDefinedAt(pred)

  def apply(pred: String): Set[Set[PureAtom]] = underlying.getOrElse(pred, Set.empty)

  def apply(pred: Predicate): Set[Set[PureAtom]] = apply(pred.head)

  def apply(call: PredCall): Set[Set[PureAtom]] = EmptyPredicates.instantiatedPureConstraints(call.args, apply(call.name))

}

object EmptyPredicates extends HarrshLogging {

  def apply(sid: SidLike): EmptyPredicates = {
    logger.debug("Will compute constraints for empty unfoldings of " + sid)

    val initialMap: Map[String, Set[Set[PureAtom]]] = (for {
      pred <- sid.preds
      if pred.rules.exists(_.isEmptyBaseRule)
    } yield baseRulesToConstraints(pred)).toMap

    val allConstraintsForEmptyModels = Combinators.fixedPointComputation[Map[String, Set[Set[PureAtom]]]](initialMap, _ == _)(step(sid))

    logger.debug("Fixed point reached. Will return constraints.")

    new EmptyPredicates(allConstraintsForEmptyModels)
  }

  private def step(sid: SidLike)(curr: Map[String, Set[Set[PureAtom]]]): Map[String, Set[Set[PureAtom]]] = {
    logger.debug("Executing a step from " + curr)

    val newEntries = for {
      pred <- sid.preds
      rule <- pred.rules
      if rule.hasCallsButNoPointers
      pure = pureConstraintsIfAllEmpty(rule, curr)
    } yield (pred.head, pure)

    newEntries.foldLeft(curr) {
      case (map, (pred, pure)) => map.updated(pred, map.getOrElse(pred, Set.empty) ++ pure)
    }
  }

  private def pureConstraintsIfAllEmpty(rule: RuleBody, prev: Map[String, Set[Set[PureAtom]]]): Set[Set[PureAtom]] = {
    if (rule.body.predCalls forall (call => prev.isDefinedAt(call.name))) {
      val pureConstraintsByCall: Seq[Set[Set[PureAtom]]] = rule.body.predCalls map instantiatedPureConstraints(prev)
      for {
        pureByCall: Seq[Set[PureAtom]] <- Combinators.choices(pureConstraintsByCall)
        combinedPureConstraints = Closure.ofAtoms(pureByCall.flatten).asSetOfAtoms
      } yield combinedPureConstraints.filter(atom => atom.l.isFree && atom.r.isFree)
    } else {
      Set.empty
    }
  }

  private def instantiatedPureConstraints(options: Map[String, Set[Set[PureAtom]]])(call: PredCall): Set[Set[PureAtom]] = {
    instantiatedPureConstraints(call.args, options.getOrElse(call.name, Set.empty))
    val renaming = Renaming.fromPairs(Var.getFvSeq(call.args.length) zip call.args)
    for {
      option <- options.getOrElse(call.name, Set.empty)
      atomsBeforeRenaming = AtomContainer(option.toSeq, Seq.empty, Seq.empty)
    } yield atomsBeforeRenaming.rename(renaming, avoidDoubleCapture = false).pure.toSet
  }

  private def baseRulesToConstraints(pred: Predicate): (String, Set[Set[PureAtom]]) = {
    (pred.head, pred.rules.filter(_.isEmptyBaseRule).map(_.body.pure.toSet).toSet[Set[PureAtom]])
  }

  def instantiatedPureConstraints(args: Seq[Var], atoms: Set[Set[PureAtom]]): Set[Set[PureAtom]] = {
    val renaming = Renaming.fromPairs(Var.getFvSeq(args.length) zip args)
    for {
      option <- atoms
      atomsBeforeRenaming = AtomContainer(option.toSeq, Seq.empty, Seq.empty)
    } yield atomsBeforeRenaming.rename(renaming, avoidDoubleCapture = false).pure.toSet
  }

}
