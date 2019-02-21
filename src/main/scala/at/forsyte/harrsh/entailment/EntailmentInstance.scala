package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive._

case class EntailmentQuerySide(sid: RichSid, calls: PredCalls, originalAssertion: SymbolicHeap)

case class EntailmentInstance(lhs: EntailmentQuerySide, rhs: EntailmentQuerySide, entailmentHolds: Option[Boolean]) {

  lazy val queryString = s"${lhs.calls} |= ${rhs.calls}"

  lazy val originalQueryString = s"${lhs.originalAssertion} |= ${rhs.originalAssertion}"

}
