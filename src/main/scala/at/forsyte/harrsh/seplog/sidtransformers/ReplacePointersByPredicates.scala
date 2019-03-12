package at.forsyte.harrsh.seplog.sidtransformers

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{FreeVar, Var}
import at.forsyte.harrsh.seplog.inductive._

object ReplacePointersByPredicates extends HarrshLogging {

  val auxPredPrefix = "pto"

  private def auxPred(arity: Int) = auxPredPrefix + arity

  private def auxPredCall(pto: PointsTo): PredCall = PredCall(auxPred(pto.to.length), pto.args)

  def apply(rootedSid: RichSid, lhs: SymbolicHeap, rhs: SymbolicHeap): (RichSid, SymbolicHeap, SymbolicHeap) = {
    val ptoByArity: Map[Int, Seq[PointsTo]] = (lhs.pointers ++ rhs.pointers).groupBy(_.to.length)
    if (ptoByArity.nonEmpty) {
      val newPreds = ptoByArity.keySet map makePtoPred
      val newRoots: Map[String, FreeVar] = ptoByArity.keySet.map(auxPred).zip(Stream.continually(Var.defaultFV(1))).toMap
      val newSid = RichSid(
        rootedSid.startPred,
        rootedSid.preds ++ newPreds,
        s"PtoPreds(${rootedSid.description})",
        rootedSid.roots ++ newRoots,
        rootedSid.sinks
      )
      val newLhs = introduceAuxPreds(lhs)
      val newRhs = introduceAuxPreds(rhs)
      logger.debug(s"Introduced auxiliary points-to predicates, yielding query $newLhs |= $newRhs wrt. SID $newSid")
      (newSid, newLhs, newRhs)
    } else {
      (rootedSid, lhs, rhs)
    }
  }

  private def introduceAuxPreds(sh: SymbolicHeap): SymbolicHeap = {
    sh.copy(pointers = Seq.empty, predCalls = sh.predCalls ++ sh.pointers.map(auxPredCall))
  }

  private def makePtoPred(arity: Int): Predicate = {
    val fvsForArity = Var.getFvSeq(arity + 1)
    val ruleBody = RuleBody(Seq.empty, SymbolicHeap(PointsTo(fvsForArity.head, fvsForArity.tail)))
    Predicate(auxPred(arity), Seq(ruleBody))
  }

}
