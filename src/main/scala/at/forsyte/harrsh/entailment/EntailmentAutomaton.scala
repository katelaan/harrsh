package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.heapautomata.{HeapAutomaton, InconsistentState}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.inductive._

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

  override def getTargetsFor(src : Seq[State], lab : SymbolicHeap) : Set[State] = ???
}

object EntailmentAutomaton extends HarrshLogging {

  def shToExtensionTypes(sh: SymbolicHeap, sid: SID) : Set[ExtensionType] = {
    assert(sh.pointers.size <= 1)
    for {
      pred <- sid.preds.toSet[Predicate]
      rule <- pred.rules
      subst <- unify(sh, rule.body)
    } yield mkExtensionType(sid, pred, rule, subst)
  }

  private def mkExtensionType(sid: SID, pred: Predicate, rule: RuleBody, subst: Substitution): ExtensionType = {
    logger.debug(s"Creating extension type from ${pred.head}, $rule, $subst")
    val singleton = UnfoldingTree.singleton(sid, pred, Some(subst))
    val withRule = singleton.unfold(singleton.root, rule)
    logger.debug(s"Unfolding tree before conversion: $withRule")
    withRule.interface.asExtensionType
  }

  private def unify(left: SymbolicHeap, right: SymbolicHeap): Set[Substitution] = ???

}
