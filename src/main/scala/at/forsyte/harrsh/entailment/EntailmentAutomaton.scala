package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.{Closure, PureEntailment}
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.Combinators

/**
  * An entailment automaton for entailment instances phi |=_sid rhs, where sid and rhs are fixed.
  * @param sid The SID definining the meaning of the rhs.
  * @param rhs The predicate call on the right-hand side of the entailment.
  */
class EntailmentAutomaton(sid: SID, rhs: PredCall) extends HeapAutomaton with InconsistentState {

  // The entailment checker only works for rooted SIDs that satisfy progress
  assert(sid.isRooted)
  assert(sid.satisfiesProgress)

  // TODO: Support for null
  assert(rhs.args forall (_.isFreeNonNull))

  override type State = Set[ExtensionType]

  override def isFinal(s: State): Boolean = {
    // A state represents all the possible ways to parse an RSH as a SID unfolding tree.
    // As long as one of those ways is a valid unfolding tree, we accept.
    s.exists(_.isFinal(rhs))
  }

  /**
    * An inconsistent state representing all "sink" states
    */
  override def inconsistentState(fvs: Seq[FreeVar]): State = Set.empty

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = {
    val localETs = EntailmentAutomaton.shToExtensionTypes(lab, sid)
    val allETs: Seq[Set[ExtensionType]] = localETs +: src
    // Compose the extension types
    val reachable = for {
      ets <- Combinators.choices(allETs map (_.toSeq))
    } yield ets.reduceLeft(_ compose _)
    // FIXME: Filter out garbage results?
    Set(reachable.toSet)
  }
}

object EntailmentAutomaton extends HarrshLogging {

  def shToExtensionTypes(sh: SymbolicHeap, sid: SID) : Set[ExtensionType] = {
    assert(sh.pointers.size <= 1)
    for {
      pred <- sid.preds.toSet[Predicate]
      // Only consider rules with the same number of free variables
      // FIXME: Is that actually the correct semantics? What about the corner case that the sh contains unused free. (As is sometimes the case e.g. for base rules of predicates; consider the SLL-with-head-pointer example.)
      if pred.arity == sh.numFV
      rule <- pred.rules
      // Instantiate the rule body with the actual free vars of the left-hand side
      ruleInstance = PredCall(pred.head, sh.freeVars).toSymbolicHeap.replaceCalls(Seq(rule.body))
      subst <- matchHeaps(sh, pred.params, ruleInstance)
    } yield mkExtensionType(sid, pred, rule, subst)
  }

  private def mkExtensionType(sid: SID, pred: Predicate, rule: RuleBody, subst: Substitution): ExtensionType = {
    logger.debug(s"Creating extension type from ${pred.head}, $rule, $subst")
    val singleton = UnfoldingTree.singleton(sid, pred, Some(subst))
    val withRule = singleton.unfold(singleton.root, sid, rule)
    logger.debug(s"Unfolding tree before conversion: $withRule")
    withRule.interface.asExtensionType
  }

  private def compatiblePointers(maybeLeftPtr: Option[PointsTo], leftEqs: Closure, maybeRightPtr: Option[PointsTo]): Boolean = {
    (maybeLeftPtr, maybeRightPtr) match {
      case (Some(leftPtr), Some(rightPtr)) =>
        // Pointers are compatible if all arguments coincide (modulo the equalities defined in the left heap)
        val matchResults = for {
          (xl, xr) <- (leftPtr.args, rightPtr.args).zipped
        } yield leftEqs.getEquivalenceClass(xl).contains(xr)
        matchResults.forall(b => b)
      case (None, None) =>
        // Neither heap allocates memory => Trivial compatibility
        true
      case _ =>
        // Only one pointer is defined => Can't match
        false
    }
  }

  // TODO: Substitution should simply become a seq rather than a map, since the domain of the substitution is determined by the rule. We can then drop the ruleFVs param here
  private def matchHeaps(leftHeap: SymbolicHeap, ruleFVs: Seq[FreeVar], rightRuleInstance: SymbolicHeap): Set[Substitution] = {
    // TODO: This equality- and unification-based reasoning should have significant overlap with existing code. (Model checking, tracking etc.) Should clean all that up and provide a set of common core routines!
    // TODO: Explicitly include the != null constraints for allocation?
    val leftEqs = Closure.ofAtoms(leftHeap.pure)
    val rightEqs = Closure.ofAtoms(rightRuleInstance.pure)

    if (
      PureEntailment.check(leftEqs, rightEqs)
      && compatiblePointers(leftHeap.pointers.headOption, leftEqs, rightRuleInstance.pointers.headOption)
    ) {
      // FIXME: Generate all possible substiutions (involving all ways that vars are allowed to alias without contradicting the RHS closure) rather than just one default substitution
      val varsToArgs = (ruleFVs, rightRuleInstance.freeVars).zipped
      val varsToSubst = varsToArgs map {
        case (v,a) => (v, leftEqs.getEquivalenceClass(v).collect{
          case fv:FreeVar => fv
        })
      }
      Set(Substitution(varsToSubst.toMap))
    } else {
      Set.empty
    }
  }

}
