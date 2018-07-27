package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.UnfoldingTree.Labeling
import at.forsyte.harrsh.modelchecking.Model
import at.forsyte.harrsh.seplog.inductive.{Rule, SID}

object MaxModelCheck {

  type MaxMCRes = (Model, UnfoldingTree)

  def apply(model: Model, pred: String, sid: SID): MaxMCRes = {
    assert(sid.predIdents.contains(pred))
    val ut = UnfoldingTree.fromPredicate(sid, pred, Map.empty)
    apply(model, ut, sid)
  }

  def apply(model: Model, ut: UnfoldingTree, sid: SID): MaxMCRes = ???

  def applicableRules(model: Model, pred: String, sid : SID): Seq[(Rule, Labeling)] = {
    val rules = sid.preds(pred).rules
    rules.flatMap(possibleMatchings(model,_))
  }

  def possibleMatchings(model: Model, rule: Rule): Set[(Rule, Labeling)] = {
    ???
  }

}
