package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.util.StringUtils

case class EntailmentQuerySide(sid: RichSid, topLevelConstraint: TopLevelConstraint, originalAssertion: SymbolicHeap) {
  def prettyPrint: String = {
    topLevelConstraint.toString + " w.r.t.\n" + sid.prettyPrint + s"\n  (derived from $originalAssertion)"
  }
}

case class EntailmentInstance(lhs: EntailmentQuerySide, rhs: EntailmentQuerySide, entailmentHolds: Option[Boolean]) {

  override def toString: String = prettyPrint

  def prettyPrint: String = {
    val idt = StringUtils.indent(4)_
    val status = entailmentHolds match {
      case None => "unknown"
      case Some(b) => ""+b
    }
    s"EntailmentInstance {\n  LHS {\n${idt(lhs.prettyPrint)}\n  }\n  RHS {\n${idt(rhs.prettyPrint)}\n  }\n  Status: $status}"
  }

  def sidPropertiesToString: String = {
    s"Properties{\n  LHS: ${querySidePropsToString(lhs)}\n  RHS: ${querySidePropsToString(rhs)}\n}"
  }

  private def querySidePropsToString(side: EntailmentQuerySide): String = {
    val sid = side.sid
    s" rooted=${sid.isRooted}, reversed=${sid.isReverseRooted}, focused=${sid.isFocused}"
  }

  lazy val queryString = s"${lhs.topLevelConstraint} |= ${rhs.topLevelConstraint}"

  lazy val originalQueryString = s"${lhs.originalAssertion} |= ${rhs.originalAssertion}"

  def usesDefaultFVs: Boolean = {
    val defaultNamesInPreds = (lhs.sid.preds ++ rhs.sid.preds) forall {
      p => p.params == Var.getFvSeq(p.params.length)
    }
    defaultNamesInPreds
  }

}
