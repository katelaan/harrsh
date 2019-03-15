package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.VarConstraints.DiseqConstraint
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec

sealed trait ConstraintUpdater extends HarrshLogging {

  def apply(vs: Set[Var]): Set[Var]

  def apply(cs: VarConstraints): Option[VarConstraints]

  def unsafeUpdate(constraints: VarConstraints): VarConstraints = apply(constraints).get

  def applyToUsage(usage: VarUsageByLabel, mayMergeClasses: Boolean): Option[VarUsageByLabel] = {
    if (!mayMergeClasses) {
      Some(usage.map{
        case (k,v) => (apply(k),v)
      })
    } else {
      mergeAwareUsageUpdate(usage)
    }
  }

  private def mergeAwareUsageUpdate(usage: VarUsageByLabel): Option[VarUsageByLabel] = {
    val grouped: Map[Set[Var], Map[Set[Var], VarUsage]] = usage.groupBy(pair => apply(pair._1))

    def doubleAlloc(group: Map[Set[Var], VarUsage]) : Boolean = group.values.count(_ == VarAllocated) >= 2

    if (grouped.values.exists(doubleAlloc)) {
      logger.debug(s"Update via $this failed because of double allocation (${grouped.values.filter(doubleAlloc)})")
      None
    }
    else {
      val res = grouped map {
        case (updated, allUsagesForUpdated) => updated -> allUsagesForUpdated.values.max
      }
      logger.trace(s"Grouped $usage into\n$grouped\n=> Resulting usage info $res")
      Some(res)
    }
  }

  protected def updateDiseqs(diseqs: Set[DiseqConstraint]): Set[DiseqConstraint] = {
    diseqs map (_.update(this))
  }

}

object ConstraintUpdater extends HarrshLogging {

  def closeUnderEquivalenceClasses(map: Map[Var, Set[Var]], classes: Iterable[Set[Var]]): SubstitutionUpdate = {
    var prevMap: Map[Var, Set[Var]] = Map.empty
    var currMap: Map[Var, Set[Var]] = map

    val step: Map[Var, Set[Var]] = classes.flatten.map{
      v:Var => v -> (classes.filter(_.contains(v)).flatten.toSet + v)
    }.toMap

    while (prevMap != currMap) {
      prevMap = currMap
      currMap = for {
        (k, vs) <- prevMap
      } yield (k, vs ++ vs flatMap (v => step.getOrElse(v, Set(v))))
    }
    logger.debug(s"Equivalence classes ${classes.mkString(", ")} extended\n$map to:\n$currMap")
    SubstitutionUpdateMap(currMap)
  }

}

case class MergeUpdate(fstClasses: Set[Set[Var]], sndClasses: Set[Set[Var]]) extends ConstraintUpdater {

  private val classesToMerge = fstClasses ++ sndClasses

  private val updateMap = MergeUpdate.makeDisjointMap(classesToMerge)

  override def apply(vs: Set[Var]): Set[Var] = updateMap(vs)

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    for {
      newUsage <- applyToUsage(cs.usage, mayMergeClasses = true)
      nowEnsured = updateDiseqs(cs.ensuredDiseqs)
      if nowEnsured forall (!_.isContradictory)
      nowSpeculatedDiseqs = updateDiseqs(cs.speculativeDiseqs) -- nowEnsured
      if nowSpeculatedDiseqs forall (!_.isContradictory)
      nowSpeculatedEqs = cs.speculativeEqs filterNot holdsAfterUpdate
    } yield VarConstraints(newUsage, nowEnsured, nowSpeculatedDiseqs, nowSpeculatedEqs)
  }

  private def holdsAfterUpdate(eq: (Var, Var)): Boolean = {
    val classOfLeft = updateMap.keys.find(_.contains(eq._1))
    updateMap(classOfLeft.get).contains(eq._2)
  }

}

object MergeUpdate extends HarrshLogging {

  def makeDisjointMap(classesToMerge: Set[Set[Var]]): Map[Set[Var], Set[Var]] = {
    val merged = mergeNonDisjoint(classesToMerge)
    (for {
      originalClass <- classesToMerge
    } yield originalClass -> merged.find(originalClass subsetOf _).get).toMap
  }

  @tailrec
  def mergeNonDisjoint(classes: Set[Set[Var]]): Set[Set[Var]] = {
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

  def mergeUsingUpdate(fst: VarConstraints, snd: VarConstraints, updater: MergeUpdate): Option[VarConstraints] = {
    logger.debug(s"Will try to merge constraints:\n$fst and\n$snd using\n$updater")
    for {
      fstUpd <- updater(fst)
      sndUpd <- updater(snd)
      _ = assert(fstUpd.allocedVars.intersect(sndUpd.allocedVars).isEmpty)
      allClasses = fstUpd.classes ++ sndUpd.classes
      newUsage = allClasses.map(c => (c, Set(fstUpd.usage.getOrElse(c,VarUnused), sndUpd.usage.getOrElse(c,VarUnused)).max)).toMap
      allEnsured = fstUpd.ensuredDiseqs ++ sndUpd.ensuredDiseqs ++ VarConstraints.diseqsImpliedByAllocation(newUsage)
      // Note: Crucially, we check whether the missing equalities are ensured *before* the update
      // After the update, they are ensured by definition, as they will have been propagated into the classes!
      missingEqs = fst.speculativeEqsNotEnsuredIn(snd) ++ snd.speculativeEqsNotEnsuredIn(fst)
    } yield VarConstraints(
      newUsage,
      allEnsured,
      (fstUpd.speculativeDiseqs ++ sndUpd.speculativeDiseqs) -- allEnsured,
      missingEqs
    )
  }

}

case class InstantiationUpdate(instantiation: Seq[(Var, Var)], classes: Set[Set[Var]]) extends ConstraintUpdater {

  private val instantiationMap = instantiation.toMap
  private def instantiationFun(v: Var) = instantiationMap.getOrElse(v, v)

  val map1 = classes.map(cls => (cls, cls map instantiationFun)).toMap
  val map2 = MergeUpdate.makeDisjointMap(map1.values.toSet)

  override def apply(vs: Set[Var]): Set[Var] = map2(map1(vs))

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    // TODO: Continue here. This can merge classes, but it's not exactly like the merge update since we completely rename stuff
    ???
  }

//  private def makeDisjoint(groupedNonDisjoint: Map[Set[Var], Iterable[Set[Var]]]): Map[Set[Var], Iterable[Set[Var]]] = {
//    Combinators.counts(groupedNonDisjoint.keys.toSeq.flatten).find{_._2 > 1} match {
//      case None =>
//        // Already disjoint
//        groupedNonDisjoint
//      case Some((v,_)) =>
//        val (overlapping, disjoint) = groupedNonDisjoint.partition(_._1.contains(v))
//        val newEntry: (Set[Var], Iterable[Set[Var]]) = (overlapping.keySet.flatten, overlapping.values.flatten)
//        val merged = disjoint + newEntry
//        logger.trace(s"Merged $groupedNonDisjoint into\n$merged")
//        makeDisjoint(merged)
//    }
//  }

}

case object TrivialUpdate extends ConstraintUpdater {

  override def apply(vs: Set[Var]): Set[Var] = vs

  override def apply(cs: VarConstraints): Option[VarConstraints] = Some(cs)

}

case class SpeculativeUpdate(speculation: Iterable[PureAtom], originalClasses: Set[Set[Var]]) extends ConstraintUpdater {

  val (speculatedEqs, speculatedDiseqs) = speculation.partition(_.isEquality)

  assert(speculatedEqs forall {
    atom => !PlaceholderVar.isPlaceholder(atom.l) && !PlaceholderVar.isPlaceholder(atom.r)
  },
    s"Placeholders in speculation $this")

  private val map: Map[Set[Var], Set[Var]] = {
    val initialPairs = originalClasses zip originalClasses
    assert(initialPairs forall (p => p._1 == p._2))
    val finalPairs = speculatedEqs.foldLeft(initialPairs) {
      case (pairs, atom) =>
        val lclass = pairs.find(_._2.contains(atom.l)).get._2
        val rclass = pairs.find(_._2.contains(atom.r)).get._2
        val combined = lclass union rclass
        pairs map {
          pair => if (pair._2 == lclass || pair._2 == rclass) (pair._1, combined) else pair
        }
    }
    val res = finalPairs.toMap
    logger.debug(s"Speculative update $this leads to update map\n$res")
    res
  }

  override def apply(vs: Set[Var]): Set[Var] = map(vs)

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    for {
      newUsage <- applyToUsage(cs.usage, mayMergeClasses = true)
      orderedEqs = speculatedEqs.map(_.ordered).map(atom => (atom.l, atom.r))
      allSpeculativeEqs = cs.speculativeEqs ++ orderedEqs
      // The speculative equalities may invalidate some of the speculative disequalities
      // We thus remove the now-ensured equalities from the speculative disequalities
      nowEnsured = updateDiseqs(cs.ensuredDiseqs)
      if nowEnsured forall (!_.isContradictory)
      nowSpeculatedDiseqs = newSpeculatedDiseqs(newUsage, cs.speculativeDiseqs, nowEnsured)
      if nowSpeculatedDiseqs forall (!_.isContradictory)
      _ = logger.trace(s"Considering $speculation for speculation wrt $cs:\nNow have speculative equalities $allSpeculativeEqs and disequalities $nowSpeculatedDiseqs.")
    } yield VarConstraints(newUsage, nowEnsured, nowSpeculatedDiseqs, allSpeculativeEqs)

  }

  private def newSpeculatedDiseqs(newUsage: VarUsageByLabel, oldSpeculativeDiseqs: Set[DiseqConstraint], nowEnsured: Set[DiseqConstraint]): Set[DiseqConstraint] = {
    val classes = newUsage.keys
    def classOf(v: Var) = classes.find(_.contains(v)).get
    val renamedSpeculated = updateDiseqs(oldSpeculativeDiseqs)
    (renamedSpeculated ++ speculatedDiseqs.map {
      case PureAtom(l, r, _) => DiseqConstraint(Set(classOf(l), classOf(r)))
    }) -- nowEnsured
  }

}

case class BijectiveRenamingUpdate(description: String, renaming: Var => Var) extends ConstraintUpdater {

  override def toString: String = s"BijectiveRenaming($description)"

  override def apply(vs: Set[Var]): Set[Var] = vs map renaming

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    applyToUsage(cs.usage, mayMergeClasses = false) map { newUsage =>
      VarConstraints(
        newUsage,
        updateDiseqs(cs.ensuredDiseqs),
        updateDiseqs(cs.speculativeDiseqs),
        cs.speculativeEqs map updateEq
      )
    }
  }

  private def updateEq(eq: (Var,Var)): (Var,Var) = {
    val pair@(l,r) = (renaming(eq._1), renaming(eq._2))
    if (l <= r) pair else pair.swap
  }

}



object BijectiveRenamingUpdate {

  def fromPairs(pairs: Seq[(Var,Var)]): BijectiveRenamingUpdate = {
    val asMap = pairs.toMap
    val renaming: Var => Var = v => asMap.getOrElse(v, v)
    BijectiveRenamingUpdate(pairs.mkString(", "), renaming)
  }

}

case class DropperUpdate(varsToDrop: Set[Var]) extends ConstraintUpdater {

  override def apply(vs: Set[Var]): Set[Var] = vs filterNot varsToDrop

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    for {
      newUsage <- applyToUsage(cs.usage, mayMergeClasses = false)
      newSpeculativeDiseqs <- updateSpeculativeDiseqs(cs.speculativeDiseqs)
      newSpeculativeEqs <- updateSpeculativeEqs(cs.speculativeEqs)
    } yield
      VarConstraints(
        newUsage filterKeys(_.nonEmpty),
        updateAndDropEmptyDiseqs(cs.ensuredDiseqs),
        newSpeculativeDiseqs,
        newSpeculativeEqs
      )
  }

  private def updateSpeculativeEqs(eqs: Set[(Var,Var)]): Option[Set[(Var,Var)]] = {
    if (eqs.exists(p => varsToDrop.contains(p._1) || varsToDrop.contains(p._2))) {
      logger.debug(s"Discarding constraints: Executing $this would lose speculative equality among $eqs")
      None
    }
    else {
      Some(eqs)
    }
  }

  private def updateSpeculativeDiseqs(diseqs: Set[DiseqConstraint]): Option[Set[DiseqConstraint]] = {
    val updated = updateDiseqs(diseqs)
    if (updated.exists(_.isVacuous)) {
      logger.debug(s"Discarding constraints: Executing $this would lose speculative disequality among $diseqs")
      None
    } else {
      Some(updated)
    }
  }

  private def updateAndDropEmptyDiseqs(diseqs: Set[DiseqConstraint]): Set[DiseqConstraint] = {
    updateDiseqs(diseqs) filterNot (_.isVacuous)
  }

}

//case class DropperUpdate(varsToDrop: Set[Var], allowDroppingSpeculation: Boolean) extends ConstraintUpdater {
//  // TODO: I'm not sure allowing to drop speculation ever makes sense (since speculation should never involve placeholders)
//
//  override def apply(vs: Set[Var]): Set[Var] = vs filterNot varsToDrop
//
//  override def apply(cs: VarConstraints): Option[VarConstraints] = {
//    for {
//      newUsage <- applyToUsage(cs.usage, mayMergeClasses = false)
//      newSpeculativeDiseqs <- updateSpeculativeDiseqs(cs.speculativeDiseqs)
//      newSpeculativeEqs <- updateSpeculativeEqs(cs.speculativeEqs)
//    } yield
//      VarConstraints(
//        newUsage filterKeys(_.nonEmpty),
//        updateAndDropEmptyDiseqs(cs.ensuredDiseqs),
//        newSpeculativeDiseqs,
//        newSpeculativeEqs
//      )
//  }
//
//  private def updateSpeculativeEqs(eqs: Set[(Var,Var)]): Option[Set[(Var,Var)]] = {
//    if (allowDroppingSpeculation) {
//      Some(eqs filterNot (p => varsToDrop.contains(p._1) || varsToDrop.contains(p._2)))
//    } else {
//      if (eqs.exists(p => varsToDrop.contains(p._1) || varsToDrop.contains(p._2)))
//        None
//      else
//        Some(eqs)
//    }
//  }
//
//  private def updateSpeculativeDiseqs(diseqs: Set[DiseqConstraint]): Option[Set[DiseqConstraint]] = {
//    if (allowDroppingSpeculation) {
//      Some(updateAndDropEmptyDiseqs(diseqs))
//    } else {
//      val updated = updateDiseqs(diseqs)
//      if (updated.exists(_.isVacuous)) {
//        None
//      } else {
//        Some(updated)
//      }
//    }
//  }
//
//  private def updateAndDropEmptyDiseqs(diseqs: Set[DiseqConstraint]): Set[DiseqConstraint] = {
//    updateDiseqs(diseqs) filterNot (_.isVacuous)
//  }
//
//}