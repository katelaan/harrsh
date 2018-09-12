package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.FreeVar
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, preds : Seq[Predicate], description : String) extends HarrshLogging {

  override def toString: String = {
    val predStrings = preds.map(_.toString.lines.map(line => s"    $line").mkString("\n"))
    description + " (start predicate '" + startPred + "'): " + predStrings.mkString("\n", " ; \n", "")
  }

  private lazy val predMap : Map[String,Predicate] = preds.map(p => p.head -> p).toMap

  lazy val predIdents: Set[String] = predMap.keySet

  def apply(predName: String): Predicate = predMap(predName)

  lazy val isRooted: Boolean = preds.forall(_.isRooted)

  lazy val satisfiesGeneralizedProgress: Boolean = {
    val violatingRules = rulesViolatingProgress
    if (violatingRules.nonEmpty) {
      logger.info(s"SID violates progress:\n${violatingRules.mkString("\n")}")
    }
    violatingRules.isEmpty
  }

  private def rulesViolatingProgress = {
    for {
      pred <- preds
      rule <- pred.rules
      if !rule.satisfiesGeneralizedProgress(pred.rootParam)
    } yield (pred, rule)
  }

  lazy val rulesWithoutPointers : Seq[(Predicate, RuleBody)] = preds.flatMap(p => p.rules.collect{
    case rule if !rule.hasPointer => (p, rule)
  })

  def arity(pred: String): Int = predMap.get(pred).map(_.arity).getOrElse(0)

  def callToPred(pred: String): SymbolicHeap = apply(pred).defaultCall

  def hasRuleForStartPred: Boolean = predMap.isDefinedAt(startPred)

  lazy val callToStartPred: SymbolicHeap = callToPred(startPred)

  def toHarrshFormat : Seq[String] = {
    val rulesWithStartFirst : Seq[(String,RuleBody)] = orderedRulesStartingInStartPred
    val undelimitedLines = rulesWithStartFirst map (pair => predRulePairToHarrshFormatLine(pair._1, pair._2))
    undelimitedLines.init.map(_ + " ;") :+ undelimitedLines.last
  }

  private def predRulePairToHarrshFormatLine(predHead: String, rule: RuleBody): String = {
    val sh = rule.body
    // In Harrsh format, we have to use default names, ignoring the names in the rule object
    val namedBody = SymbolicHeap.toHarrshFormat(sh, Naming.DefaultNaming)
    s"$predHead <= $namedBody"
  }

  private def orderedRulesStartingInStartPred: Seq[(String, RuleBody)] = {
    val startRules: Seq[(String, RuleBody)] = apply(startPred).rules.map(rb => (startPred, rb))
    val otherRules = for {
      pred <- preds
      if pred.head != startPred
      rule <- pred.rules
    } yield (pred.head, rule)
    startRules ++ otherRules
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

  def fromSymbolicHeap(sh: SymbolicHeap, backgroundSID: SID = SID.empty) : SID = {
    val startPred = "sh"
    val newRule = RuleBody(sh.boundVars.toSeq map (_.toString), sh)
    val newPred = Predicate(startPred, Seq(newRule))
    backgroundSID.copy(startPred = startPred, preds = newPred +: backgroundSID.preds, description = "symbolic heap")
  }

  implicit val sidToLatex: ToLatex[SID] = (a: SID, _: Naming) => {
    val predStrings = for {
      pred <- a.preds
      if pred.rules.nonEmpty
    } yield predToLatex(pred)
    predStrings.mkString("\n\n")
  }

  private def predToLatex(pred: Predicate): String = {
    val rulesStr = for {
      rule <- pred.rules
    } yield s"$$ ${pred.head} \\Longleftarrow ${rule.body.toLatex(rule.naming)} $$\n"
    rulesStr.mkString("\n\n")
  }

}