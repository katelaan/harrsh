package at.forsyte.harrsh

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.RuleBody
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

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

  type SubstitutionUpdate = Var => Set[Var]

  object SubstitutionUpdate extends HarrshLogging {

    def unionUpdate(fst: VarUsageByLabel, snd: VarUsageByLabel): SubstitutionUpdate = {
      val equivalenceClasses = fst.keySet ++ snd.keySet
      val alloced: Set[Var] = (fst.toSet ++ snd).collect{
        case (vs, VarAllocated) => vs
      }.flatten
      val merged = mergeNonDisjoint(equivalenceClasses, alloced)
      (for {
        set <- merged
        key <- set
      } yield key -> set).toMap
    }

    @tailrec
    private def mergeNonDisjoint(classes: Set[Set[Var]], alloced: Set[Var]): Set[Set[Var]] = {
      val counts = Combinators.counts(classes.toSeq.flatten)
      counts.find(_._2 > 1) match {
        case None =>
          logger.debug("Returning disjoint classes " + classes)
          classes
        case Some((v,_)) =>
          val (withV, withoutV) = classes.partition(_.contains(v))
          logger.debug("Merging non-disjoint classes " + withV)
          mergeNonDisjoint(withoutV + withV.flatten, alloced)
      }
    }

    def fromPairs(pairs: Seq[(Var,Var)]): SubstitutionUpdate = {
      val map = pairs.toMap
      v => Set(map.getOrElse(v, v))
    }

    def fromUnification(unification: Unification): SubstitutionUpdate = {
      v => unification.filter(_.contains(v)).flatten.toSet + v
    }

    def fromRuleAndParamSubstitution(rule: RuleBody, paramSubst: Substitution): SubstitutionUpdate = {
      val freeVarsToLeafSubst = rule.body.freeVars.zip(paramSubst.toSeq).toMap
      val boundVars = rule.body.boundVars.toSeq
      val allUnusedPlaceholders = PlaceholderVar.allUnusedPlaceholders(used = paramSubst.placeholders)
      val boundVarsToPlaceholders = boundVars.zip(allUnusedPlaceholders).toMap
      val renameArgs: SubstitutionUpdate = {
        case fv: FreeVar => freeVarsToLeafSubst(fv)
        case NullConst => Set(NullConst)
        case bv: BoundVar => Set[Var](boundVarsToPlaceholders(bv))
      }
      renameArgs
    }

    def redundantPlaceholderDropper(nodeLabels: Iterable[ContextPredCall]): SubstitutionUpdate = {
      def getRedundantVars(vs: Set[Var]): Set[Var] = {
        val (phs, nonPhs) = vs.partition(PlaceholderVar.isPlaceholder)
        if (nonPhs.nonEmpty) {
          // There is a proper free var in this equivalence class => discard all equivalent placeholders
          phs
        } else {
          // Keep only the smallest placeholder among multiple placeholders
          val typedPhs = phs map (ph => PlaceholderVar.fromVar(ph).get)
          phs - PlaceholderVar.min(typedPhs).toFreeVar
        }
      }
      val equivalenceClasses = Substitution.extractVarEquivClasses(nodeLabels map (_.subst))
      val redundantVars = equivalenceClasses.flatMap(getRedundantVars)
      logger.trace(s"Redundant vars: $redundantVars")

      val updateF: SubstitutionUpdate = {
        v => if (redundantVars.contains(v)) Set.empty else Set(v)
      }
      updateF
    }

  }

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

  object VarUsageByLabel extends HarrshLogging {

    def isWellFormed(usageInfo: VarUsageByLabel): Boolean = {
      Combinators.counts(usageInfo.keys.toSeq.flatten).forall(_._2 == 1)
    }

    def update(usageInfo: VarUsageByLabel, f: Var => Set[Var]): VarUsageByLabel = {
      val grouped: Map[Set[Var], Map[Set[Var], VarUsage]] = usageInfo.groupBy(pair => pair._1.flatMap(f))
      val res = grouped map {
        case (updated, allUsagesForUpdated) => updated -> allUsagesForUpdated.values.max
      }
      logger.trace(s"Grouped $usageInfo into\n$grouped\n=> Resulting usage info $res")
      res
    }

    def merge(u1: VarUsageByLabel, u2: VarUsageByLabel): VarUsageByLabel = {
      val keys = u1.keySet ++ u2.keySet
      val pairs: Set[(Set[Var], VarUsage)] = keys.map{
        k => (k, Seq(u1.getOrElse(k, VarUnused), u2.getOrElse(k, VarUnused)).max)
      }
      pairs.toMap
    }

    def combineUsageInfo(fst: VarUsageByLabel, snd: VarUsageByLabel, update: SubstitutionUpdate): VarUsageByLabel = {
      val fstUpdated = VarUsageByLabel.update(fst, update)
      val sndUpdated = VarUsageByLabel.update(snd, update)
      merge(fstUpdated, sndUpdated)
    }

    def restrictToOccurringLabels(usageInfo: VarUsageByLabel, decomp: ContextDecomposition): VarUsageByLabel = {
      val occurringVarSets = decomp.occurringLabels
      restrictToOccurringLabels(usageInfo, occurringVarSets)
    }

    def restrictToOccurringLabels(usageInfo: VarUsageByLabel, decomp: Iterable[EntailmentContext]): VarUsageByLabel = {
      // TODO Code duplication w.r.t. ContextDecomposition.occurringLabels
      val occurringVarSets = decomp.toSet[EntailmentContext].flatMap(_.labels).flatMap(_.subst.toSeq)
      restrictToOccurringLabels(usageInfo, occurringVarSets)
    }

    def restrictToOccurringLabels(usageInfo: VarUsageByLabel, occurringVarSets: Set[Set[Var]]): VarUsageByLabel = {
      usageInfo.filterKeys(occurringVarSets)
    }

    // TODO: [DEAD] Drop this once we get rid of the old notion of contexts
    def restrictToSubstitutionsInLabels(usageInfo: VarUsageByLabel, nodeLabels: Iterable[ContextPredCall]): VarUsageByLabel = {
      val occurringSubstitutions: Set[Set[Var]] = nodeLabels.toSet[ContextPredCall].flatMap(_.subst.toSeq)
      usageInfo.filter(pair => occurringSubstitutions.contains(pair._1))
    }

  }

}
