package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var.Naming
import at.forsyte.harrsh.seplog.sidtransformers
import at.forsyte.harrsh.util.ToLatex
import at.forsyte.harrsh.util.ToLatex._

trait SidLike extends HarrshLogging {

  val startPred : String
  val preds : Seq[Predicate]
  val description : String

  protected lazy val predMap : Map[String,Predicate] = preds.map(p => p.head -> p).toMap

  lazy val predIdents: Set[String] = predMap.keySet

  def apply(predName: String): Predicate = predMap(predName)

  def arity(pred: String): Int = predMap.get(pred).map(_.arity).getOrElse(0)

  def callToPred(pred: String): SymbolicHeap = apply(pred).defaultCall

  def hasRuleForStartPred: Boolean = predMap.isDefinedAt(startPred)

  lazy val callToStartPred: SymbolicHeap = callToPred(startPred)

  lazy val rulesWithoutPointers : Seq[(Predicate, RuleBody)] = preds.flatMap(p => p.rules.collect{
    case rule if !rule.hasPointer => (p, rule)
  })

  lazy val predsThatOccurAtMostOnceInUnfolding: Set[Predicate] = {
    // FIXME: A proper implementation of this that also works for non-auxiliary predicates?
    preds.filter(sidtransformers.isAuxiliaryPred).toSet
  }

  def toHarrshFormat : Seq[String] = {
    val rulesWithStartFirst : Seq[(String,RuleBody)] = orderedRulesStartingInStartPred
    val undelimitedLines = rulesWithStartFirst map (pair => predRulePairToHarrshFormatLine(pair._1, pair._2))
    undelimitedLines.init.map(_ + " ;") :+ undelimitedLines.last
  }

  private def predRulePairToHarrshFormatLine(predHead: String, rule: RuleBody): String = {
    val sh = rule.body
    // In Harrsh format, we have to use default names, ignoring the names in the rule object
    val namedBody = SymbolicHeap.toHarrshFormat(sh, Naming.DefaultHarrshNaming)
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

object SidLike {

  implicit val sidToLatex: ToLatex[SidLike] = (a: SidLike, _: Naming) => {
    val rulesStr = for {
      pred <- a.preds
      rule <- predToLatex(pred)
    } yield rule
    """\[\begin{array}{rcl}""" + rulesStr.mkString("\n", "\\\\\n", "\n") + """\end{array}\]"""
  }

  private def predToLatex(pred: Predicate): Seq[String] = {
    for {
      rule <- pred.rules
    } yield s"  \\RuleName{${pred.head}} &\\Longleftarrow& ${rule.body.toLatex(rule.naming)}"
  }

}
