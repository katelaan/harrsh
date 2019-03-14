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

//  object VarUsageByLabel extends HarrshLogging {
//
//    def isWellFormed(usageInfo: VarUsageByLabel): Boolean = {
//      Combinators.counts(usageInfo.keys.toSeq.flatten).forall(_._2 == 1)
//    }
//
//    def update(usageInfo: VarUsageByLabel, f: Var => Set[Var]): VarUsageByLabel = {
//      val grouped: Map[Set[Var], Map[Set[Var], VarUsage]] = usageInfo.groupBy(pair => pair._1.flatMap(f))
//      val res = grouped map {
//        case (updated, allUsagesForUpdated) => updated -> allUsagesForUpdated.values.max
//      }
//      logger.trace(s"Grouped $usageInfo into\n$grouped\n=> Resulting usage info $res")
//      res
//    }
//
//    def merge(u1: VarUsageByLabel, u2: VarUsageByLabel): VarUsageByLabel = {
//      val keys = u1.keySet ++ u2.keySet
//      val pairs: Set[(Set[Var], VarUsage)] = keys.map{
//        k => (k, Seq(u1.getOrElse(k, VarUnused), u2.getOrElse(k, VarUnused)).max)
//      }
//      pairs.toMap
//    }
//
//    def combineUsageInfo(fst: VarUsageByLabel, snd: VarUsageByLabel, update: SubstitutionUpdate): VarUsageByLabel = {
//      val fstUpdated = VarUsageByLabel.update(fst, update)
//      val sndUpdated = VarUsageByLabel.update(snd, update)
//      merge(fstUpdated, sndUpdated)
//    }
//
//    def restrictToOccurringLabels(usageInfo: VarUsageByLabel, decomp: ContextDecomposition): VarUsageByLabel = {
//      val occurringVarSets = decomp.occurringLabels
//      restrictToOccurringLabels(usageInfo, occurringVarSets)
//    }
//
//    def restrictToOccurringLabels(usageInfo: VarUsageByLabel, occurringVarSets: Set[Set[Var]]): VarUsageByLabel = {
//      usageInfo.filterKeys(vs => occurringVarSets(vs) || vs.exists(PlaceholderVar.isNonPlaceholderFreeVar))
//    }
//
//    // TODO: [DEAD] Drop this once we get rid of the old notion of contexts
//    def restrictToSubstitutionsInLabels(usageInfo: VarUsageByLabel, nodeLabels: Iterable[ContextPredCall]): VarUsageByLabel = {
//      val occurringSubstitutions: Set[Set[Var]] = nodeLabels.toSet[ContextPredCall].flatMap(_.subst.toSeq)
//      usageInfo.filter(pair => occurringSubstitutions.contains(pair._1))
//    }
//
//  }

}
