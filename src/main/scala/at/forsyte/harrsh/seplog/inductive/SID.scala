package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.Var.Naming

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, preds : Seq[Predicate], description : String) {

  override def toString: String = {
    description + " (start predicate '" + startPred + "'): " + preds.mkString("\n    ", " ; \n    ", "")
  }

  private lazy val predMap : Map[String,Predicate] = preds.map(p => p.head -> p).toMap

  lazy val predIdents: Set[String] = predMap.keySet

  def apply(predName: String): Predicate = predMap(predName)

  def isRooted: Boolean = preds.forall(_.rootParam.nonEmpty)

  def arity(pred: String): Int = predMap.get(pred).map(_.arity).getOrElse(0)

  def callToPred(pred: String): SymbolicHeap = apply(pred).defaultCall

  def hasRuleForStartPred: Boolean = predMap.isDefinedAt(startPred)

  lazy val callToStartPred: SymbolicHeap = callToPred(startPred)

  def toHarrshFormat : Seq[String] = {
    val startRules: Seq[(String, RuleBody)] = apply(startPred).rules.map(rb => (startPred, rb))
    val otherRules = for {
      pred <- preds
      if pred.head != startPred
      rule <- pred.rules
    } yield (pred.head, rule)
    val rulesWithStartFirst : Seq[(String,RuleBody)] = startRules ++ otherRules
    val undelimitedLines = for {
      (head, rule) <- rulesWithStartFirst
      sh = rule.body
      // In Harrsh format, we have to use default names, ignoring the names in the rule object
      namedBody = SymbolicHeap.toHarrshFormat(sh, Naming.DefaultNaming)
    } yield s"$head <= $namedBody"
    undelimitedLines.init.map(_ + " ;") :+ undelimitedLines.last
  }

}

object SID extends HarrshLogging {

  def fromTuples(startPred: String, ruleTuples: Seq[(String,RuleBody)], description: String, rootParams: Map[String, FreeVar] = Map.empty): SID = {
    val rulesByPred = ruleTuples.groupBy(_._1)
    val rules = rulesByPred.map(grouped => Predicate(
      grouped._1,
      Predicate.alignFVSeqs(grouped._2.map(_._2)), rootParams.get(grouped._1))
    ).toSeq
    SID(startPred, rules, description)
  }

  def apply(startPred: String, description: String, rootParams: Map[String, FreeVar], ruleTuples: (String, Seq[String], SymbolicHeap)*): SID = {
    fromTuples(startPred, ruleTuples map (t => (t._1, RuleBody(t._2, t._3))), description, rootParams)
  }

  def apply(startPred: String, description: String, ruleTuples: (String, Seq[String], SymbolicHeap)*): SID = {
    fromTuples(startPred, ruleTuples map (t => (t._1, RuleBody(t._2, t._3))), description)
  }

  def empty(startPred : String) : SID = SID(startPred, Seq.empty[Predicate], "")

  def empty : SID = SID("X", Seq.empty[Predicate], "")

  def fromTopLevelSH(sh: SymbolicHeap, sid: SID) : SID = {
    val startPred = "sh"
    val newRule = RuleBody(sh.boundVars.toSeq map (_.toString), sh)
    val newPred = Predicate(startPred, Seq(newRule))
    sid.copy(startPred = startPred, preds = newPred +: sid.preds, description = "symbolic heap")
  }

}