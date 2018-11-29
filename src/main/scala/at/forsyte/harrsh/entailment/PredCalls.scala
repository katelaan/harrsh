package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}
import at.forsyte.harrsh.util.Combinators

case class PredCalls(calls: Seq[PredCall]) extends HarrshLogging {

  lazy val size = calls.size
  lazy val names = calls map (_.name)
  lazy val orderedNames = names.sorted
  lazy val paramsByName: Map[String, Seq[Seq[Var]]] = calls.groupBy(_.name).map{
    case (name, matches) => (name, matches.map(_.args))
  }

  override def toString: String = calls.mkString(" * ")

  def toSymbolicHeap = SymbolicHeap(calls:_*)

  def implies(other: Set[ContextPredCall]): Boolean = {
    if (orderedNames != other.toSeq.map(_.pred.head).sorted) {
      false
    } else {
      matchableAgainst(other)
    }
  }

  private def matchableAgainst(other: Set[ContextPredCall]): Boolean = {
    // TODO: Cache this in the decomposition?
    val otherByName: Map[String, Seq[Substitution]] = other.groupBy(_.pred.head).map{
      case (name, matches) => (name, matches.map(_.subst).toSeq)
    }

    val res = PredCalls.callsMatchableWithoutRenaming(paramsByName, otherByName)
    logger.debug(s"$this matchable against $other? ===> $res")
    res
  }

}

object PredCalls {

  def callsMatchableWithoutRenaming(lhs: Map[String, Seq[Seq[Var]]], rhs: Map[String, Seq[Substitution]]): Boolean = {
    assert(lhs.keySet == rhs.keySet)
    lhs.keys.forall(pred => callargsAndSubstsMatch(lhs(pred), rhs(pred)))
  }

  private def callargsAndSubstsMatch(lhsArgs: Seq[Seq[Var]], rhsSubsts: Seq[Substitution]): Boolean = {
    Combinators.permutations(lhsArgs).exists(argsSeqImplySubstSeq(_, rhsSubsts))
  }

  private def argsSeqImplySubstSeq(lhsLinearization: Seq[Seq[Var]], rhsSubsts: Seq[Substitution]): Boolean = {
    (lhsLinearization, rhsSubsts).zipped.forall{
      case (args,subst) => argsImplySubst(args, subst)
    }
  }

  private def argsImplySubst(args: Seq[Var], subst: Substitution): Boolean = {
    (args, subst.toSeq).zipped.forall{
      case (arg, substVal) => substVal.contains(arg)
    }
  }

//  def callImpliesContext(call: PredCall, root: ContextPredCall): Boolean = {
//    (call.args, root.subst.toSeq).zipped.forall{
//      case (arg, substVal) => substVal.contains(arg)
//    }
//  }

}