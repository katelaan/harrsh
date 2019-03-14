package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

sealed trait SubstitutionUpdate extends (Var => Set[Var]) {

  /**
    * Extend the codomain by closing it under the given equivalence classes.
    * @param classes
    */
  def closeUnderEquivalenceClasses(classes: Iterable[Set[Var]]): SubstitutionUpdate

  def apply(vs: Set[Var]): Set[Var] = vs flatMap apply

}

case class SubstitutionUpdateMap(underlying: Map[Var, Set[Var]]) extends SubstitutionUpdate with HarrshLogging {

  override def apply(v: Var): Set[Var] = underlying.getOrElse(v, Set(v))

  override def closeUnderEquivalenceClasses(classes: Iterable[Set[Var]]): SubstitutionUpdate = {
    var prevMap: Map[Var, Set[Var]] = Map.empty
    var currMap: Map[Var, Set[Var]] = underlying
    while (prevMap != currMap) {
      prevMap = currMap
      currMap = for {
        (k, vs) <- prevMap
      } yield (k, vs ++ vs flatMap prevMap)
    }
    logger.debug(s"Equivalence classes ${classes.mkString(", ")} extended\n$underlying to:\n$currMap")
    SubstitutionUpdateMap(currMap)
  }
}

case class RenamingToFreshVarsUpdate(underlying: Var => Var) extends SubstitutionUpdate {

  override def apply(v: Var): Set[Var] = Set(underlying(v))

  override def closeUnderEquivalenceClasses(classes: Iterable[Set[Var]]): SubstitutionUpdate = {
    // Nothing to do, since we're introducing new names.
    this
  }
}

object SubstitutionUpdate extends HarrshLogging {
  def unionUpdate(fst: VarConstraints, snd: VarConstraints): SubstitutionUpdate = {
    val equivalenceClasses = fst.classes ++ snd.classes
    val merged = mergeNonDisjoint(equivalenceClasses)
    SubstitutionUpdateMap((for {
      set <- merged
      key <- set
    } yield key -> set).toMap)
  }

  @tailrec
  private def mergeNonDisjoint(classes: Set[Set[Var]]): Set[Set[Var]] = {
    val counts = Combinators.counts(classes.toSeq.flatten)
    counts.find(_._2 > 1) match {
      case None =>
        logger.trace("Returning disjoint classes " + classes)
        classes
      case Some((v,_)) =>
        val (withV, withoutV) = classes.partition(_.contains(v))
        logger.trace("Merging non-disjoint classes " + withV)
        mergeNonDisjoint(withoutV + withV.flatten)
    }
  }

  def fromPairs(pairs: Seq[(Var,Var)]): SubstitutionUpdate = {
    SubstitutionUpdateMap(pairs.toMap.mapValues(Set(_)))
  }

  def fromSetsOfEqualVars(classes: Iterable[Set[Var]]): SubstitutionUpdate = {
    val initialMap: Map[Var, Set[Var]] = classes.flatten.map{
      v:Var => v -> (classes.filter(_.contains(v)).flatten.toSet + v)
    }.toMap

    SubstitutionUpdateMap(initialMap).closeUnderEquivalenceClasses(classes)
//    var prevMap: Map[Var, Set[Var]] = Map.empty
//    var currMap: Map[Var, Set[Var]] = classes.flatten.map{
//      v:Var => v -> (classes.filter(_.contains(v)).flatten.toSet + v)
//    }.toMap
//    while (prevMap != currMap) {
//      prevMap = currMap
//      currMap = for {
//        (k, vs) <- prevMap
//      } yield (k, vs flatMap prevMap)
//    }
//    logger.debug(s"Equivalence classes ${classes.mkString(", ")} lead to the following updates:\n$currMap")
//    SubstitutionUpdateMap(currMap)
  }

//  def fromRuleAndParamSubstitution(rule: RuleBody, paramSubst: Substitution): SubstitutionUpdate = {
//    val freeVarsToLeafSubst = rule.body.freeVars.zip(paramSubst.toSeq).toMap
//    val boundVars = rule.body.boundVars.toSeq
//    val allUnusedPlaceholders = PlaceholderVar.allUnusedPlaceholders(used = paramSubst.placeholders)
//    val boundVarsToPlaceholders = boundVars.zip(allUnusedPlaceholders).toMap
//    val renameArgs: SubstitutionUpdate = {
//      case fv: FreeVar => freeVarsToLeafSubst(fv)
//      case NullConst => Set(NullConst)
//      case bv: BoundVar => Set[Var](boundVarsToPlaceholders(bv))
//    }
//    renameArgs
//  }

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

    SubstitutionUpdateMap(redundantVars.zip(Stream.continually(Set.empty[Var])).toMap)
  }

}
