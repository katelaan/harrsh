package at.forsyte.harrsh.converters

import at.forsyte.harrsh.main.EntailmentQuery
import at.forsyte.harrsh.seplog.inductive._
import at.forsyte.harrsh.seplog.sidtransformers.RestrictSidToCalls

object ToSlideFormat extends EntailmentFormatConverter {

  override def apply(filename: String, pr: EntailmentQuery): Seq[(String,String)] = {
    val lhsSid = RestrictSidToCalls(pr.sid, pr.lhs.predCalls.toSet)
    val rhsSid = RestrictSidToCalls(pr.sid, pr.rhs.predCalls.toSet)
    val lhsFile = filename + ".lhs.pred"
    val rhsFile = filename + ".rhs.pred"
    Seq(
      (lhsFile, toSlideSid(pr.lhs, lhsSid)),
      (rhsFile, toSlideSid(pr.rhs, rhsSid)),
      (filename + ".callargs", s"$lhsFile $rhsFile")
    )
  }

  private def toSlideSid(toplevel: SymbolicHeap, sid: Sid): String = {
    val callString = slideRootCall(toplevel)
    val preds = sid.preds map toSlidePred
    (callString +: preds).mkString("\n\n")
  }

  private def toSlidePred(pred: Predicate): String = {
    val slideRules = pred.bodySHs map shToSlideSh
    s"${pred.defaultCall} ::= ${slideRules.mkString(" | ")}"
  }

  private def shToSlideSh(sh: SymbolicHeap): String = {
    val prefix = if (sh.boundVars.isEmpty) "" else {
      s"\\E ${sh.boundVars.mkString(",")} . "
    }
    val ptrs = sh.pointers map {
      case PointsTo(from, to) => s"$from->${to.mkString(",")}"
    }
    val calls = sh.predCalls map (_.toString)
    val pure = sh.pure map {
      // TODO: Do they assume implicit disequalities or not?
      case PureAtom(l, r, isEquality) => l.toString + (if (isEquality) "=" else "!=") + r
    }
    // TODO: The delimiter between pure formulas is just guess work
    val pureSuffix = if (pure.isEmpty) "" else " & " + pure.mkString(", ")
    val spatial = (ptrs ++ calls).mkString(" * ")
    val spatialOrEmp = if (spatial.isEmpty) "emp" else spatial
    val shString = prefix + spatialOrEmp + pureSuffix
    sanitize(shString)
  }

  private def slideRootCall(toplevel: SymbolicHeap): String = s"RootCall ${shToSlideSh(toplevel)}"

}
