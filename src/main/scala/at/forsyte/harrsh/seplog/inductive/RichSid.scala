package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.FreeVar

case class RichSid(override val startPred : String,
                   override val preds : Seq[Predicate],
                   override val description : String,
                   roots: Map[String, FreeVar]) extends SidLike
{
  lazy val isRooted: Boolean = roots.size == preds.size

  lazy val hasEmptyBaseRules: Boolean = emptyBaseRules.nonEmpty

  lazy val rootParamIndex: Map[String, Int] = {
    for {
      (predIdent, pred) <- predMap
      root <- roots.get(predIdent)
    } yield (predIdent, pred.params.indexOf(root))
  }

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
      if !rule.satisfiesGeneralizedProgress(roots.get(pred.head))
    } yield (pred, rule)
  }

  lazy val emptyBaseRules: Seq[(Predicate, RuleBody)] = {
    for {
      pred <- preds
      rule <- pred.rules
      if !(rule.body.hasPointer || rule.body.predCalls.nonEmpty)
    } yield (pred, rule)
  }

  def underlying: Sid = Sid(startPred, preds, description)

}

object RichSid {

  def empty : RichSid = RichSid("X", Seq.empty[Predicate], "", Map.empty)

}