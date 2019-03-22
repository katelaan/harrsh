package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment
import at.forsyte.harrsh.entailment.VarConstraints.{DiseqConstraint, PartitionedDiseqs, RewrittenBoundVarDiseqConstraint}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.util.Combinators

import scala.annotation.tailrec
import scala.collection.mutable

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

  def holdsInUsage(usage: VarUsageByLabel, eq: (Var, Var)): Boolean = {
    usage.keys.exists{
      vs => vs.contains(eq._1) && vs.contains(eq._2)
    }
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
      nowRewritten <- VarConstraints.updateRewrittenConstraints(this, newUsage, cs.rewrittenSpeculation)
    } yield VarConstraints(newUsage, nowEnsured, nowSpeculatedDiseqs, nowSpeculatedEqs, nowRewritten)
  }

  private def holdsAfterUpdate(eq: (Var, Var)): Boolean = {
    // Is the right var in the updated class of the left var?
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
      doubleAlloc = fstUpd.allocedVars.intersect(sndUpd.allocedVars)
      _ = if (doubleAlloc.nonEmpty) logger.debug(s"Trying to merge $fstUpd with $sndUpd, but they both allocate $doubleAlloc => Can't compose")
      if doubleAlloc.isEmpty
      allClasses = fstUpd.classes ++ sndUpd.classes
      newUsage = allClasses.map(c => (c, Set(fstUpd.usage.getOrElse(c,VarUnused), sndUpd.usage.getOrElse(c,VarUnused)).max)).toMap
      allEnsured = fstUpd.ensuredDiseqs ++ sndUpd.ensuredDiseqs ++ VarConstraints.diseqsImpliedByAllocation(newUsage)
      // Note: Crucially, we check whether the missing equalities are ensured *before* the update
      // After the update, they are ensured by definition, as they will have been propagated into the classes!
      missingEqs = fst.speculativeEqsNotEnsuredIn(snd) ++ snd.speculativeEqsNotEnsuredIn(fst)
      nowRewritten <- VarConstraints.updateRewrittenConstraints(updater, newUsage, fst.rewrittenSpeculation ++ snd.rewrittenSpeculation)
    } yield VarConstraints(
      newUsage,
      allEnsured,
      (fstUpd.speculativeDiseqs ++ sndUpd.speculativeDiseqs) -- allEnsured,
      missingEqs,
      nowRewritten
    )
  }

  def mergeWithUnifyingUpdate(fst: VarConstraints, snd: VarConstraints): Option[VarConstraints] = {
    val update = MergeUpdate(fst.classes, snd.classes)
    mergeUsingUpdate(fst, snd, update)
  }

}

case class InstantiationUpdate(instantiation: Seq[(Var, Var)], classes: Set[Set[Var]]) extends ConstraintUpdater {

  private val instantiationMap = instantiation.toMap
  private def instantiationFun(v: Var) = instantiationMap.getOrElse(v, v)

  val map1 = classes.map(cls => (cls, cls map instantiationFun)).toMap
  val map2 = MergeUpdate.makeDisjointMap(map1.values.toSet)

  override def apply(vs: Set[Var]): Set[Var] = map2(map1(vs))

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    // TODO: Reduce code duplication wrt MergeUpdate. This only differs in the treatment of speculative equalities
    for {
      newUsage <- applyToUsage(cs.usage, mayMergeClasses = true)
      nowEnsured = updateDiseqs(cs.ensuredDiseqs)
      if nowEnsured forall (!_.isContradictory)
      nowSpeculatedDiseqs = updateDiseqs(cs.speculativeDiseqs) -- nowEnsured
      if nowSpeculatedDiseqs forall (!_.isContradictory)
      // Only difference to merge update
      nowSpeculatedEqs = cleanSpeculativeEqs(cs)
      newRewritten <- VarConstraints.updateRewrittenConstraints(this, newUsage, cs.rewrittenSpeculation)
    } yield VarConstraints(newUsage, nowEnsured, nowSpeculatedDiseqs, nowSpeculatedEqs, newRewritten)
  }

  private def cleanSpeculativeEqs(cs: VarConstraints) : Set[(Var,Var)] = {
    // Only drop speculative equalities if made equal explicitly by the update
    // It's *not* sound to check whether the equality holds after the update -- all do by definition!
    cs.speculativeEqs map instantiateEquality filterNot (eq => eq._1 == eq._2) map {
      eq => if (eq._1 < eq._2) eq else eq.swap
    }
  }

  private def instantiateEquality(eq: (Var, Var)): (Var, Var) = {
    (instantiationFun(eq._1), instantiationFun(eq._2))
  }

}

case class SpeculativeUpdate(atoms: Iterable[PureAtom], originalClasses: Set[Set[Var]], assumeWithoutSpeculation: Set[Var] = Set.empty) extends ConstraintUpdater {

  logger.debug{
    if (atoms.nonEmpty)
      s"Will add $atoms to speculation (unless already ensured)"
    else
      "No atoms to add."
  }

  val (eqs, diseqs) = atoms.partition(_.isEquality)

  private val map: Map[Set[Var], Set[Var]] = {
    val initialPairs = originalClasses zip originalClasses
    assert(initialPairs forall (p => p._1 == p._2))
    val finalPairs = eqs.foldLeft(initialPairs) {
      case (pairs, atom) =>
        logger.debug(s"Will update $pairs using $atom")
        val lclass = pairs.find(_._2.contains(atom.l)).get._2
        val rclass = pairs.find(_._2.contains(atom.r)).get._2
        val combined = lclass union rclass
        pairs map {
          pair => if (pair._2 == lclass || pair._2 == rclass) (pair._1, combined) else pair
        }
    }
    val res = finalPairs.toMap
    logger.debug(s"Atoms $atoms lead to update map\n$res")
    res
  }

  override def apply(vs: Set[Var]): Set[Var] = map.getOrElse(vs, vs)

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    for {
      newUsage <- applyToUsage(cs.usage, mayMergeClasses = true)
      // Equalities involving placeholders can always be assumed and thus need not be added to speculation
      orderedNonplaceholderEqs = eqs.filterNot(atom => PlaceholderVar.isPlaceholder(atom.l) || PlaceholderVar.isPlaceholder(atom.r)).map(_.ordered).map(atom => (atom.l, atom.r))
      orderedEqs = if (assumeWithoutSpeculation.isEmpty) orderedNonplaceholderEqs else {
        orderedNonplaceholderEqs.filterNot(pair => assumeWithoutSpeculation.contains(pair._1) || assumeWithoutSpeculation.contains(pair._2))
      }
      // Important: Check wrt *old* usage info, in the new usage info the equalities do, of course, hold!
      newOrderedEqs = orderedEqs filterNot (ConstraintUpdater.holdsInUsage(cs.usage,_))
      allSpeculativeEqs = cs.speculativeEqs ++ newOrderedEqs
      // The speculative equalities may invalidate some of the speculative disequalities
      // We thus remove the now-ensured equalities from the speculative disequalities
      nowEnsured = updateDiseqs(cs.ensuredDiseqs)
      if nowEnsured forall (!_.isContradictory)
      nowSpeculatedDiseqs = newSpeculatedDiseqs(newUsage, cs.speculativeDiseqs, nowEnsured)
      if nowSpeculatedDiseqs forall (!_.isContradictory)
      newRewritten <- VarConstraints.updateRewrittenConstraints(this, newUsage, cs.rewrittenSpeculation)
      _ = logger.trace(s"Considering $atoms as additional constraints wrt $cs:\nNow have speculative equalities $allSpeculativeEqs and disequalities $nowSpeculatedDiseqs.")
    } yield VarConstraints(newUsage, nowEnsured, nowSpeculatedDiseqs, allSpeculativeEqs, newRewritten)

  }

  private def newSpeculatedDiseqs(newUsage: VarUsageByLabel, oldSpeculativeDiseqs: Set[DiseqConstraint], nowEnsured: Set[DiseqConstraint]): Set[DiseqConstraint] = {
    val classes = newUsage.keys
    def classOf(v: Var) = classes.find(_.contains(v)).get
    val renamedSpeculated = updateDiseqs(oldSpeculativeDiseqs)
    (renamedSpeculated ++ diseqs.map {
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
        cs.speculativeEqs map updateEq,
        // Bijective => Can't fail
        VarConstraints.updateRewrittenConstraints(this, newUsage, cs.rewrittenSpeculation).get
      )
    }
  }

  private def updateEq(eq: (Var,Var)): (Var,Var) = {
    val pair@(l,r) = (renaming(eq._1), renaming(eq._2))
    if (l <= r) pair else pair.swap
  }

}

object BijectiveRenamingUpdate extends HarrshLogging {

  def fromPairs(pairs: Seq[(Var,Var)]): BijectiveRenamingUpdate = {
    val asMap = pairs.toMap
    val renaming: Var => Var = v => asMap.getOrElse(v, v)
    BijectiveRenamingUpdate(pairs.mkString(", "), renaming)
  }

  def placeholderNormalFormUpdater(orderedNodeLabels: Seq[ContextPredCall]): ConstraintUpdater = {
    logger.trace("Will order placeholders in order of occurrence in " + orderedNodeLabels)
    val found = mutable.Set.empty[PlaceholderVar]
    val order = new mutable.ListBuffer[PlaceholderVar]()
    for {
      nodeLabel <- orderedNodeLabels
      vs <- nodeLabel.subst.toSeq
      v <- vs
      ph <- PlaceholderVar.fromVar(v)
      if !found.contains(ph)
    } {
      order.append(ph)
      found.add(ph)
    }
    val renameFrom = order map (_.toFreeVar)
    val renameTo = (1 to order.size) map (PlaceholderVar(_).toFreeVar)
    val pairs = renameFrom.zip(renameTo)
    logger.trace("Will establish placeholder normalform via update " + pairs)
    fromPairs(pairs)
  }

}

case class DropperUpdate(varsToDrop: Set[Var]) extends ConstraintUpdater {

  override def apply(vs: Set[Var]): Set[Var] = vs filterNot varsToDrop

  override def apply(cs: VarConstraints): Option[VarConstraints] = {
    for {
      newUsage <- applyToUsage(cs.usage, mayMergeClasses = false)
      (newSpeculativeDiseqs, newRewrittenDiseqs) <- updateSpeculativeDiseqs(cs)
      newSpeculativeEqs <- updateSpeculativeEqs(cs.speculativeEqs)
      updatedRewritten <- VarConstraints.updateRewrittenConstraints(this, newUsage, cs.rewrittenSpeculation)
      newRewritten = updatedRewritten ++ newRewrittenDiseqs
    } yield
      VarConstraints(
        newUsage filterKeys(_.nonEmpty),
        updateAndDropEmptyDiseqs(cs.ensuredDiseqs),
        newSpeculativeDiseqs,
        newSpeculativeEqs,
        newRewritten
      )
  }

  private def updateSpeculativeEqs(eqs: Set[(Var,Var)]): Option[Set[(Var,Var)]] = {
    // FIXME: Try to express speculative eqs using non-dropped vars!
    if (eqs.exists(p => varsToDrop.contains(p._1) || varsToDrop.contains(p._2))) {
      logger.debug(s"Discarding constraints: Executing $this would lose speculative equality among $eqs")
      None
    }
    else {
      Some(eqs)
    }
  }

  private def updateSpeculativeDiseqs(cs: VarConstraints): Option[(Set[DiseqConstraint], Set[RewrittenBoundVarDiseqConstraint])] = {
    val PartitionedDiseqs(unaffected, newRewritten, nonrewritable) = VarConstraints.splitSpeculativeDiseqsOnDroppedVars(cs, varsToDrop)
    if (nonrewritable.nonEmpty) {
      logger.debug(s"Discarding constraints: Executing $this would lose speculative disequalities ${nonrewritable.mkString(", ")}")
      None
    } else {
      Some((unaffected map (_.update(this)), newRewritten map (_.update(this))))
    }
  }

//  private def updateSpeculativeDiseqs(diseqs: Set[DiseqConstraint]): Option[Set[DiseqConstraint]] = {
//    //val PartitionedDiseqs(unaffected, newRewritten, nonrewritable) = VarConstraints.splitSpeculativeDiseqsOnDroppedVars(this, varsToDrop)
//
//    val updated = updateDiseqs(diseqs)
//    if (updated.exists(_.isVacuous)) {
//      logger.debug(s"Discarding constraints: Executing $this would lose speculative disequality among $diseqs")
//      None
//    } else {
//      Some(updated)
//    }
//  }

  private def updateAndDropEmptyDiseqs(diseqs: Set[DiseqConstraint]): Set[DiseqConstraint] = {
    updateDiseqs(diseqs) filterNot (_.isVacuous)
  }

}

case class ChainedUpdater(fst: ConstraintUpdater, snd: ConstraintUpdater) extends ConstraintUpdater {

  override def apply(vs: Set[Var]): Set[Var] = snd(fst(vs))

  override def apply(cs: VarConstraints): Option[VarConstraints] = for {
    intermediate <- fst(cs)
    updated <- snd(intermediate)
  } yield updated

}