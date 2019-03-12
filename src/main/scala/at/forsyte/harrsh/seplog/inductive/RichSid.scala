package at.forsyte.harrsh.seplog.inductive

import at.forsyte.harrsh.seplog.inductive.RichSid.{FocusedVar, RootFocus, SinkFocus}
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.util.Combinators

case class RichSid(override val startPred : String,
                   override val preds : Seq[Predicate],
                   override val description : String,
                   roots: Map[String, FreeVar],
                   sinks: Map[String, FreeVar] = Map.empty) extends SidLike
{
  lazy val isRooted: Boolean = roots.size == preds.size
  lazy val isReverseRooted: Boolean = sinks.size == preds.size
  lazy val isFocused: Boolean = (roots ++ sinks).size == preds.size
  lazy val hasMixedFocus: Boolean = isFocused && !isRooted && !isReverseRooted

  lazy val hasEmptyBaseRules: Boolean = predsWithEmptyModels.nonEmpty
  lazy val hasRecursiveRulesWithoutPointers: Boolean = preds.exists(pred => pred.rules.exists(_.hasCallsButNoPointers))

  def focus(head: String): FocusedVar = {
    roots.get(head).map(FocusedVar(_, RootFocus)).getOrElse(FocusedVar(sinks(head), SinkFocus))
  }

  lazy val rootParamIndex: Map[String, Int] = {
    for {
      (predIdent, pred) <- predMap
      root <- roots.get(predIdent)
    } yield (predIdent, pred.params.indexOf(root))
  }

  lazy val predsWithEmptyModels = EmptyPredicates(this)

  def hasEmptyModels(pred: Predicate): Boolean = predsWithEmptyModels.hasEmptyModels(pred.head)

  def constraintOptionsForEmptyModels(pred: Predicate): Set[Set[PureAtom]] = predsWithEmptyModels(pred)

  lazy val empClosedNonProgressRules : Set[(Predicate, RuleBody)] = for {
    p <- preds.toSet[Predicate]
    rule <- p.rules
    if rule.hasCallsButNoPointers
    empClosedRule <- empClose(p.head, rule)
  } yield (p, empClosedRule)

  private def empClose(head: String, rule: RuleBody) = {
    def possibleCallReplacements(call: PredCall): Set[Set[SepLogAtom]] = {
      predsWithEmptyModels(call).map(_.map(_.asInstanceOf[SepLogAtom])) + Set(call)
    }

    val optionsByCall: Seq[Set[Set[SepLogAtom]]] = rule.body.predCalls map possibleCallReplacements
    for {
      newAtomsByCall: Seq[Set[SepLogAtom]] <- Combinators.choices(optionsByCall)
      allAtoms = rule.body.pure ++ newAtomsByCall.flatten
      newSh = SymbolicHeap(rule.body.freeVars, allAtoms:_*)
      numCalls = newSh.predCalls.size
      if numCalls > 0 && (numCalls > 1 || newSh.predCalls.head.name != head)
    } yield RuleBody(rule.qvarNames, newSh)

  }

  def underlying: Sid = Sid(startPred, preds, description)

  override def toString: String = underlying.toString

  def prettyPrint: String = underlying.toString

}

object RichSid {

  def fromSid(sid: SidLike, roots: Map[String, FreeVar], sinks: Map[String, FreeVar] = Map.empty): RichSid = {
    RichSid(sid.startPred, sid.preds, sid.description, roots, sinks)
  }

  def empty : RichSid = RichSid("X", Seq.empty[Predicate], "", Map.empty)

  sealed trait FocusDirection {
    val name: String
    def ptoArgsForDirection(pto: PointsTo): Set[Var]
  }
  case object RootFocus extends FocusDirection {
    override def ptoArgsForDirection(pto: PointsTo): Set[Var] = Set(pto.from)

    override val name: String = "root"
  }
  case object SinkFocus extends FocusDirection {
    override def ptoArgsForDirection(pto: PointsTo): Set[Var] = pto.to.toSet

    override val name: String = "sink"
  }
  case class FocusedVar(fv: FreeVar, dir: FocusDirection)

}