package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{FreeVar, Var}

case class Predicate(head: String, rules: Seq[Rule]) {
  // All rules of the predicate have the same number of parameters
  // Note that this is ensured when constructing predicate via Predicate.fromRules
  assert(rules.map(_.body.numFV).distinct.length == 1)

  def arity: Int = rules.head.body.numFV

  def defaultCall: SymbolicHeap = {
    val params = rules.head.body.freeVars
    SymbolicHeap(Seq.empty, Seq.empty, Seq(PredCall(head, params)), params)
  }

  def ruleBodies: Seq[SymbolicHeap] = rules map (_.body)

}

object Predicate {

  def fromRules(rules: Iterable[Rule]): Predicate = {
    // The rules are all for the same predicate
    assert(rules.toSeq.map(_.head).distinct.size == 1)

    val numfv = arity(rules)
    val paddedRules = rules map padToArity(numfv)

    Predicate(rules.head.head, paddedRules.toSeq)
  }

  private def padToArity(arity: Int)(rule: Rule) : Rule = {
    if (rule.body.numFV < arity) {
      // Don't reuse either string identifiers or "internal" FVs
      val usedVars = rule.body.freeVars.toSet[Var]
      val newVars = Var.freshFreeVars(usedVars, arity - rule.body.numFV)
      rule.copy(body = rule.body.copy(freeVars = rule.body.freeVars ++ newVars))
    } else {
      rule
    }
  }

  private def arity(rules: Iterable[Rule]): Int = {
    // Note that we take the maximum here, because we allow that some of the rules do not mention all FVs
    // (and in particular not the maxFV; see also DefaultSID parser)
    if (rules.isEmpty) 0 else rules.map(_.body.numFV).max
  }

}
