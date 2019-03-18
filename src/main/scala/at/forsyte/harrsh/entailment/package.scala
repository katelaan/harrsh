package at.forsyte.harrsh

import at.forsyte.harrsh.seplog.Var

package object entailment {

  case class DoubleAllocException(msg: String) extends Exception(msg)

  type NodeId = Int

  object NodeId {

    def freshIds(usedIds: Set[NodeId], numIds: Int): Seq[NodeId] = {
      val maxUsed = if (usedIds.nonEmpty) usedIds.max else -1
      maxUsed + 1 to maxUsed + numIds
    }

    def zero: NodeId = 0

  }

  type Unification = Seq[Set[Var]]

  sealed trait VarUsage {
    def isUsed: Boolean = this match {
      case VarUnused => false
      case VarAllocated => true
      case VarReferenced => true
    }

    def shortString: String = this match {
      case VarUnused => "unused"
      case VarAllocated => "alloced"
      case VarReferenced => "refed"
    }
  }

  case object VarUnused extends VarUsage
  case object VarAllocated extends VarUsage
  case object VarReferenced extends VarUsage

  object VarUsage {
    implicit val ord: Ordering[VarUsage] = Ordering.fromLessThan[VarUsage]{
      (left, right) => (left, right) match {
        case (VarUnused, VarAllocated) => true
        case (VarUnused, VarReferenced) => true
        case (VarReferenced, VarAllocated) => true
        case _ => false
      }
    }
    val unused: VarUsage = VarUnused
    val allocated: VarUsage = VarAllocated
    val referenced: VarUsage = VarReferenced
  }

  type VarUsageInfo = Seq[VarUsage]

  type VarUsageByLabel = Map[Set[Var], VarUsage]

}
