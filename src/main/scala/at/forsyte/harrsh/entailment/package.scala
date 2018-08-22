package at.forsyte.harrsh

import at.forsyte.harrsh.entailment.VarUsage.{Allocated, Referenced, Unused}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, FreeVar, NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.RuleBody

package object entailment {

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

    def fromPairs(pairs: Seq[(Var,Var)]): SubstitutionUpdate = {
      val map = pairs.toMap
      v => Set(map.getOrElse(v, v))
    }

    def fromUnification(unification: Unification): SubstitutionUpdate = {
      v => unification.find(_.contains(v)).getOrElse(Set(v))
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

    def redundantPlaceholderDropper(nodeLabels: Iterable[NodeLabel]): SubstitutionUpdate = {
      def getRedundantVars(vs: Set[Var]): Set[Var] = {
        val (phs, nonPhs) = vs.partition(PlaceholderVar.isPlaceholder)
        if (nonPhs.nonEmpty) {
          // There is a proper free var in this equivalence class => discard all equivalent placeholders
          phs
        } else {
          // Keep only the smalles placeholder among multiple placeholders
          val typedPhs = phs map (ph => PlaceholderVar.fromVar(ph).get)
          phs - PlaceholderVar.min(typedPhs).toFreeVar
        }
      }
      val equivalenceClasses = Substitution.extractVarEquivClasses(nodeLabels map (_.subst))
      val redundantVars = equivalenceClasses.flatMap(getRedundantVars)
      logger.debug(s"Reundant vars: $redundantVars")

      val updateF: SubstitutionUpdate = {
        v => if (redundantVars.contains(v)) Set.empty else Set(v)
      }
      updateF
    }

  }

  sealed trait VarUsage {
    def isUsed: Boolean = this match {
      case Unused => false
      case Allocated => true
      case Referenced => true
    }
  }
  object VarUsage {
    implicit val ord: Ordering[VarUsage] = Ordering.fromLessThan[VarUsage]{
      (left, right) => (left, right) match {
        case (Unused, Allocated) => true
        case (Unused, Referenced) => true
        case (Referenced, Allocated) => true
        case _ => false
      }
    }

    case object Unused extends VarUsage
    case object Allocated extends VarUsage
    case object Referenced extends VarUsage

    val unused: VarUsage = Unused
    val allocated: VarUsage = Allocated
    val referenced: VarUsage = Referenced
  }

  type VarUsageInfo = Seq[VarUsage]

  type VarUsageByLabel = Map[Set[Var], VarUsage]

  object VarUsageByLabel {

    def update(m: VarUsageByLabel, f: Var => Set[Var]): VarUsageByLabel = {
      m map {
        case (set, usage) => (set.flatMap(f), usage)
      }
    }

  }

}
