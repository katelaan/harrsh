package at.forsyte.harrsh.entailment
import at.forsyte.harrsh.seplog.inductive.RichSid

object UnionSolver extends TopLevelSolver {
  override def checkValidity(sid: RichSid, lhsConstraint: TopLevelConstraint, rhsConstraint: TopLevelConstraint, reachable: Map[String, Set[EntailmentProfile]]): Boolean = ???
}
