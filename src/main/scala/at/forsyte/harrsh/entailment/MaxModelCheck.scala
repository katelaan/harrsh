package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.modelchecking.Model
import at.forsyte.harrsh.seplog.inductive.{PredCall, Rule, SID, SymbolicHeap}

object MaxModelCheck {

  type MaxMCRes = (Model, UnfoldingTree)

  def apply(model: Model, pred: String, sid: SID): MaxMCRes = {
    assert(sid.predicates.contains(pred))
    val ut = UnfoldingTree.fromPredicate(pred, sid)
    apply(model, ut, sid)
  }

  def apply(model: Model, ut: UnfoldingTree, sid: SID): MaxMCRes = ???

  def applicableRuleBodies(model: Model, predCall: PredCall, sid : SID): Set[SymbolicHeap] = {
    val rules = sid.predToRuleBodies(predCall.name)
    rules.filter(rule => model.evaluateReducedSymbolicHeap(rule.withoutCalls))
  }

}
