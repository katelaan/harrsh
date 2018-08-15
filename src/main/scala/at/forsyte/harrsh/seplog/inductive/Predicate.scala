package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.{FreeVar, Var}

case class Predicate(head: String, rules: Seq[RuleBody], rootParam: Option[FreeVar] = None) {

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

}

object Predicate {

  def apply(head: String, ruleBodies : (Seq[String], SymbolicHeap)*): Predicate = {
    apply(head, None, ruleBodies:_*)
  }

  def alignFVSeqs(ruleBodies: Seq[RuleBody]): Seq[RuleBody] = {
    val freeVars: Set[Seq[FreeVar]] = ruleBodies.map(_.body.freeVars).toSet
    val maxFreeVars = freeVars.maxBy(_.size)
    for {
      RuleBody(_, body) <- ruleBodies
      v <- body.freeVars
    } {
      if (!maxFreeVars.contains(v)) throw new IllegalArgumentException(s"Can't construct predicate from conflicting FV lists in $ruleBodies")
    }
    ruleBodies map (b => b.copy(body = b.body.copy(freeVars = maxFreeVars)))
  }

  def apply(head: String, rootParam: Option[FreeVar], ruleBodies : (Seq[String], SymbolicHeap)*): Predicate = {
    val rules = ruleBodies map RuleBody.tupled
    val aligned = alignFVSeqs(rules)
    Predicate(head, aligned, rootParam)
  }

}
