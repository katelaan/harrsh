package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PureAtom, SidLike}
import at.forsyte.harrsh.{Implicits, TestValues}
import at.forsyte.harrsh.test.HarrshTableTest

trait TestWithSyntacticSugarForDecompositions extends HarrshTableTest with Implicits with TestValues{

  /**
    * Context with aliasing
    */
  case class CA[A <: Var](calls: Seq[(String,Seq[Set[Var]])], usageInfo: Map[Set[A],VarUsage], ensured: Set[PureAtom], missing: Set[PureAtom] = Set.empty) {
    def toEntailmentContext(sid: SidLike): OldEntailmentContext = {
      val labels = calls map {
        case (ident, subst) => ContextPredCall(sid(ident), Substitution(subst))
      }
      val root = labels.head
      val leaves = labels.tail.toSet
      val pure = PureConstraintTracker(ensured, missing)
      val typedUsageInfo = usageInfo map {
        case (vs, usage) => (vs map (_.asInstanceOf[Var]), usage)
      }
      OldEntailmentContext(root, leaves, typedUsageInfo, pure, convertToNormalform = false)
    }
  }
  object C {
    /**
      * Overloaded constructor for context without aliasing
      */
    def apply[A <: Var](calls: Seq[(String,Seq[Var])], usageInfo: Map[A,VarUsage], ensured: Set[PureAtom], missing: Set[PureAtom] = Set.empty): CA[A] = {
      val callsWithSets = calls map {
        case (pred,vs) => (pred,vs map (Set(_)))
      }
      val usageWithSets = usageInfo map {
        case (v,usage) => (Set(v), usage)
      }
      CA(callsWithSets, usageWithSets, ensured, missing)
    }
  }

  def D[A <: Var](sid: SidLike)(cs: CA[A]*): OldContextDecomposition = OldContextDecomposition(cs.map(_.toEntailmentContext(sid)).toSet)

  def pr(pred: String, vars: Var*): (String,Seq[Var]) = (pred, vars)

  def alloc[A](vs: A*): Map[A, VarUsage] = (vs map ((_,VarUsage.allocated))).toMap
  def ref[A](vs: A*): Map[A, VarUsage] = (vs map ((_,VarUsage.referenced))).toMap
  def unused[A](vs: A*): Map[A, VarUsage] = (vs map ((_,VarUsage.unused))).toMap

}
