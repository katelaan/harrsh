package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.FreeVar

case class Predicate(head: String, rules: Seq[RuleBody]) {

  if (rules.isEmpty) {
    throw new IllegalArgumentException("Can't construct predicate without rules.")
  }

  assert(rules.map(_.body.freeVars).toSet.size == 1,
    s"Trying to construct predicate $head from rules with different free variables $rules. Preprocess with Predicate.alignFVSeqs?")

  lazy val bodySHs: Seq[SymbolicHeap] = rules map (_.body)

  lazy val arity: Int = rules.head.body.numFV

  lazy val params: Seq[FreeVar] = rules.head.body.freeVars

  def defaultCall: SymbolicHeap = {
    SymbolicHeap(Seq.empty, Seq.empty, Seq(PredCall(head, params)), params)
  }

  override def toString: String = {
    val ruleStrings = rules map {
      rule => head + rule.freeVarNames.mkString("(",", ", ")") + " <= " + rule.toString
    }
    ruleStrings.mkString("\n")
  }

  def headToLatex = Predicate.predicateHeadToLatex(head)

}

object Predicate {

  def predicateHeadToLatex(head: String) = head.replaceAllLiterally("_", "\\_")

}