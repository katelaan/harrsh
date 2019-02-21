package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Renaming}

object SidFactory extends HarrshLogging {

  def makeRootedSid(startPred: String, description: String, rootParams: Map[String, FreeVar], ruleTuples: (String, Seq[String], SymbolicHeap)*): RichSid = {
    val unrooted = makeSidfromRuleBodies(startPred, ruleTuples map (t => (t._1, RuleBody(t._2, t._3))), description)
    RichSid(unrooted.startPred, unrooted.preds, unrooted.description, rootParams)
  }

  def makeSid(startPred: String, description: String, ruleTuples: (String, Seq[String], SymbolicHeap)*): Sid = {
    makeSidfromRuleBodies(startPred, ruleTuples map (t => (t._1, RuleBody(t._2, t._3))), description)
  }

  def makeSidfromRuleBodies(startPred: String, ruleTuples: Seq[(String,RuleBody)], description: String): Sid = {
    val rulesByPred = ruleTuples.groupBy(_._1).mapValues(_.map(_._2)).toSeq
    makeSidfromPredSpecs(startPred, rulesByPred, description)
  }

  def makeSidfromPredSpecs(startPred: String, predSpecs: Seq[(String,Seq[RuleBody])], description: String): Sid = {
    val preds = predSpecs.map{
      spec => rulesToPred(spec._1, spec._2)
    }
    Sid(startPred, preds, description)
  }

  private def rulesToPred(predIdent: String, ruleBodies: Seq[RuleBody]): Predicate = {
    logger.debug(s"Will create predicate for $predIdent with rule bodies ${ruleBodies.mkString(", ")}")
    Predicate(predIdent, unifyFVNames(ruleBodies))
  }

  private def unifyFVNames(ruleBodies: Seq[RuleBody]): Seq[RuleBody] = {
    // All rules must have the same number of parameters
    assert(ruleBodies.map(_.freeVarNames.length).toSet.size == 1,
      s"Different numbers of parameters: ${ruleBodies.map(_.freeVarNames)}"
    )

    // Arbitrarily pick the parameter names of the first rule
    val freeVarSeq = ruleBodies.head.body.freeVars

    val renameFVs = (b: RuleBody) => {
      if (b.body.freeVars == freeVarSeq) {
        b
      } else {
        val renaming = Renaming.fromPairs(b.body.freeVars zip freeVarSeq)
        b.copy(body = b.body.rename(renaming, Some(freeVarSeq)))
      }
    }

    ruleBodies.head +: ruleBodies.tail.map(renameFVs)
  }

  /**
    * This factory method ensures that the predicates are assigned the union of the FVs that occur in its rules.
    *
    * This is necessary when parsing Harrsh format, as all rules in that format must use the same parameter names, but some rules may not use all parameters.
    *
    */
  def makeSidFromHarrshRules(startPred: String, ruleTuples: Seq[(String,RuleBody)], description: String /*, rootParams: Map[String, FreeVar] = Map.empty*/): Sid = {
    val rulesByPred = ruleTuples.groupBy(_._1)
    val rules = rulesByPred.map(grouped => Predicate(
      grouped._1,
      alignFVSeqs(grouped._2.map(_._2))/*,
      rootParams.get(grouped._1)*/)
    ).toSeq
    Sid(startPred, rules, description)
  }

  private def alignFVSeqs(ruleBodies: Seq[RuleBody]): Seq[RuleBody] = {
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

  def fromSymbolicHeap(sh: SymbolicHeap, backgroundSID: Sid = Sid.empty) : Sid = {
    val startPred = "sh"
    val newRule = RuleBody(sh.boundVars.toSeq map (_.toString), sh)
    val newPred = Predicate(startPred, Seq(newRule))
    backgroundSID.copy(startPred = startPred, preds = newPred +: backgroundSID.preds, description = "symbolic heap")
  }

  def shToRuleBody(sh: SymbolicHeap): RuleBody = {
    // TODO: The SH API is obviously not meant to be used in this way. Refactor?
    val withoutGaps = SymbolicHeap(sh.atoms.closeGapsInBoundVars, sh.freeVars)
    RuleBody(defaultBoundVarNames(withoutGaps), withoutGaps)
  }

  // TODO: Introduce unique names for bound vars + don't hardcode prefix?
  private def defaultBoundVarNames(sh: SymbolicHeap): Seq[String] = sh.boundVars.toSeq.map(bv => "_"+bv.index)

}
