package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main._
import at.forsyte.harrsh.seplog.Var.Naming

/**
  * System of inductive definitions
  * Created by jens on 10/15/16.
  */
case class SID(startPred : String, preds : Map[String,Predicate], description : String) {

  override def toString: String = {
    description + " (start predicate '" + startPred + "'): " + preds.toSeq.sortBy(_._1).mkString("\n    ", " ; \n    ", "")
  }

  lazy val rules: Seq[Rule] = preds.toSeq.sortBy(_._1).map(_._2).flatMap(_.rules)

  lazy val predIdents: Set[String] = preds.keySet

  def arity(pred: String): Int = preds.get(pred).map(_.arity).getOrElse(0)

  def callToPred(pred: String): SymbolicHeap = preds(pred).defaultCall

  lazy val callToStartPred: SymbolicHeap = callToPred(startPred)

  lazy val predToRuleBodies: Map[String, Seq[SymbolicHeap]] = {
    preds.map(pred => (pred._1, pred._2.ruleBodies))
  }

  def toHarrshFormat : Seq[String] = {
    val (start, rest) = rules.partition(_.head == startPred)
    val rulesWithStartFirst : Seq[Rule] = start ++ rest
    val undelimitedLines = for {
      rule <- rulesWithStartFirst
      sh = rule.body
      // In Harrsh format, we have to use default names, ignoring the names in the rule object
      namedBody = SymbolicHeap.toHarrshFormat(sh, Naming.DefaultNaming)
    } yield rule.head + " <= " + namedBody
    undelimitedLines.init.map(_ + " ;") :+ undelimitedLines.last
  }

}

object SID extends HarrshLogging {

  def apply(startPred : String, description : String, ruleTuples : (String, Seq[String], SymbolicHeap)*): SID = {
    val rules = ruleTuples map Rule.fromTuple
    SID(startPred, rulesToPreds(rules), description)
  }

  def apply(startPred: String, rules: Iterable[Rule], description: String): SID = {
    SID(startPred, rulesToPreds(rules), description)
  }

  private def rulesToPreds(rules: Iterable[Rule]): Map[String, Predicate] = {
    val rulesByPred = rules.groupBy(_.head)
    val tuples = rulesByPred map (grouped => (grouped._1, Predicate.fromRules(grouped._2)))
    Map() ++ tuples
  }

  def empty(startPred : String) : SID = SID(startPred, "")

  def empty : SID = SID("X", "")

  def fromTopLevelSH(sh: SymbolicHeap, sid: SID) : SID = {
    val startPred = "sh"
    val newRule = Rule(startPred, sh.boundVars.toSeq map (_.toString), sh)
    val newPred = Predicate(startPred, Seq(newRule))
    sid.copy(startPred = startPred, preds = sid.preds.updated(startPred, newPred), description = "symbolic heap")
  }

}