package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.util.Combinators

case class VarConstraints(usage: VarUsageByLabel, ensuredDiseqs: Set[(Set[Var], Set[Var])], speculativeDiseqs: Set[(Set[Var], Set[Var])], speculativeEqs: Set[(Var, Var)]) extends HarrshLogging {

  // TODO: The usage map / classes implicitly contain speculation (the result of identifying parameters in matching). Do we handle this correctly? Think in particular about going to the sink state or returning no state in profile composition when we filter out all decompositions because of inconsistencies
  // FIXME: It seems like the implicit equality speculation is actually unsound in case we forget one of the participating variables! Should probably track such equalities explicitly here as well!

  assert(isWellFormed, s"$this is not well-formed")

  val isSpeculative: Boolean = speculativeDiseqs.nonEmpty || speculativeEqs.nonEmpty

  lazy val allVars: Set[Var] = usage.keySet.flatten
  lazy val allocedVars: Set[Var] = usage.filter(_._2 == VarAllocated).keySet.flatten
  lazy val boundVars: Set[Var] = allVars filter (_.isBound)
  lazy val nonNullNonPlaceholderVars: Set[Var] = allVars.filterNot(v => v.isNull || PlaceholderVar.isPlaceholder(v))
  lazy val placeholders: Set[PlaceholderVar] = allVars flatMap PlaceholderVar.fromVar
  lazy val nullAlloced: Boolean = usageOfOption(NullConst).contains(VarAllocated)

  lazy val classes: Set[Set[Var]] = usage.keySet

  lazy val allDiseqs: Set[(Set[Var], Set[Var])] = ensuredDiseqs ++ speculativeDiseqs

  def isWellFormed: Boolean = {
    val noOverlaps = VarConstraints.hasDisjointEqualityClasses(usage)
    val diseqsAmongClasses = allDiseqs forall (diseq => classes.contains(diseq._1) && classes.contains(diseq._2))
    val ensuredNotSpeculative = ensuredDiseqs.intersect(speculativeDiseqs).isEmpty
    val nonEmptyClasses = classes forall (_.nonEmpty)
    val orderedSpeculation = speculativeEqs forall (p => p._1 <= p._2)
    val speculativeEqsNotPlaceholder = speculativeEqs forall (pair => !PlaceholderVar.isPlaceholder(pair._1) && !PlaceholderVar.isPlaceholder(pair._2))
    noOverlaps && diseqsAmongClasses && ensuredNotSpeculative && nonEmptyClasses && orderedSpeculation && speculativeEqsNotPlaceholder
  }

  def definedOnAllOf(varSets: Set[Set[Var]]): Boolean = varSets subsetOf classes

  def classOf(v: Var): Set[Var] = classes.find(_.contains(v)).getOrElse{
    throw new IllegalArgumentException(s"$this do not contain $v")
  }

  def classOfOption(v: Var): Option[Set[Var]] = classes.find(_.contains(v))

  def usageOf(v: Var): VarUsage = usage.find(_._1.contains(v)).getOrElse{
    throw new IllegalArgumentException(s"$this do not contain $v")
  }._2

  def usageOfOption(v: Var): Option[VarUsage] = usage.find(_._1.contains(v)).map(_._2)

  def impliesWithoutSpeculation(atoms: Iterable[PureAtom]): Boolean = atoms forall impliesWithoutSpeculation

  def impliesWithoutSpeculation(atom: PureAtom): Boolean = {
    val res = if (atom.isEquality) {
      val lclass = classOfOption(atom.l)
      lclass.nonEmpty && lclass == classOfOption(atom.r)
    } else {
      ensuredDiseqs exists (VarConstraints.impliesDiseq(_, atom.l, atom.r))
    }
    logger.debug(s"$this implies $atom without speculation: $res")
    res
  }

  def isConsistent: Boolean = {
    def contradictoryDiseq = allDiseqs.exists(diseq => diseq._1 == diseq._2)
    !nullAlloced && !contradictoryDiseq
  }

  def isInconsistent: Boolean = !isConsistent

  def forget(vs: Set[Var]): VarConstraints = {
    assume(!areRequiredInSpeculation(vs))

    val updateMap = classes.map(c => (c,c -- vs)).toMap
    val newUsage = usage.map(pair => (updateMap(pair._1), pair._2)).filterKeys(_.nonEmpty)
    val newEnsuredDiseqs = VarConstraints.updateAndDropEmptyDiseqs(ensuredDiseqs, updateMap)
    //val newSpeculation = VarConstraints.updateAndDropEmptyDiseqs(speculatedDiseqs, updateMap)
    // Because we're never allowed to lose speculative constraints in a forget operation, we don't need to check for empty constraints here.
    val newSpeculativeDiseqs = VarConstraints.updateDiseqs(speculativeDiseqs, updateMap)
    // Note: The speculative equalities need not be updated, because we assume that the vars we forget are not needed in speculation
    VarConstraints(newUsage, newEnsuredDiseqs, newSpeculativeDiseqs, speculativeEqs)
  }

  def restrictToNonPlaceholdersAnd(classes: Set[Set[Var]]): Option[VarConstraints] = {
    val varsToRetainExplicitly = classes.flatten
    val varsToRetain = allVars filter (v => varsToRetainExplicitly(v) || !PlaceholderVar.isPlaceholder(v))
    if (areRequiredInSpeculativeDiseqs(allVars -- varsToRetain)) {
      logger.debug(s"Can't restrict to $classes, because that would lose speculative information")
      None
    } else {
      Some(VarConstraints(
        usage.filterKeys(_.exists(varsToRetain)),
        ensuredDiseqs.filter(pair => pair._1.exists(varsToRetain) && pair._2.exists(varsToRetain)),
        speculativeDiseqs,
        speculativeEqs
      ))
    }
  }

  def mergeUsingUpdate(otherBeforeUpdate: VarConstraints, upd: SubstitutionUpdate): Option[VarConstraints] = {
    logger.debug(s"Will try to merge constraints:\n$this and\n$otherBeforeUpdate")
    for {
      thisUpd <- update(upd)
      other <- otherBeforeUpdate.update(upd)
      _ = assume(thisUpd.allocedVars.intersect(other.allocedVars).isEmpty)
      allClasses = thisUpd.classes ++ other.classes
      newUsage = allClasses.map(c => (c, Set(thisUpd.usage.getOrElse(c,VarUnused), other.usage.getOrElse(c,VarUnused)).max)).toMap
      allEnsured = thisUpd.ensuredDiseqs ++ other.ensuredDiseqs ++ VarConstraints.diseqsImpliedByAllocation(newUsage)
      // Note: Crucially, we check whether the missing equalities are ensured *before* the update
      // After the update, they are ensured by definition, as they will have been propagated into the classes!
      missingEqs = speculativeEqsNotEnsuredIn(otherBeforeUpdate) ++ other.speculativeEqsNotEnsuredIn(this)
    } yield VarConstraints(
      newUsage,
      allEnsured,
      (speculativeDiseqs ++ other.speculativeDiseqs) -- allEnsured,
      missingEqs
    )
  }

  def speculativeEqsNotEnsuredIn(other: VarConstraints): Set[(Var, Var)] = {
    speculativeEqs.filterNot(atom => other.impliesWithoutSpeculation(PureAtom(atom._1, atom._2, isEquality = true)))
  }

  /**
    * Returns true iff forgetting vs would lose one of the speculative constraints.
    *
    * This would be a source of unsoundness, since we'd forget that the context decomposition is only possible under
    * the assumption that somewhere in the call context we will establish the disequality.
    * (Putting one or missing constraints out of scope means the entailment can never become true.)
    *
    * @param v
    * @return
    */
  def areRequiredInSpeculation(vs: Set[Var]): Boolean = {
    // TODO: is it correct that we can actually *never* drop a speculative constraint, no matter if all vars in the constraint are forgotten? I think that's the case, so I changed the code accordingly
    areRequiredInSpeculativeEqs(vs) || areRequiredInSpeculativeDiseqs(vs)
  }

  private def areRequiredInSpeculativeEqs(vs: Set[Var]): Boolean = {
    speculativeEqs exists {
      //pair => vs.contains(pair._1) != vs.contains(pair._2)
      pair => vs.contains(pair._1) || vs.contains(pair._2)
    }
  }

  private def areRequiredInSpeculativeDiseqs(vs: Set[Var]): Boolean = {
    speculativeDiseqs exists {
      pair => (pair._1 subsetOf vs) || (pair._2 subsetOf vs)
    }

    // TODO: It's possible to do this with just one traversal of the set.
//    val diseqsToForget = speculatedDiseqs filter {
//      pair => (pair._1 subsetOf vs) || (pair._2 subsetOf vs)
//    }
//
//    // We'd forget a disequality even though one side contains a variable that we do *not* forget
//    diseqsToForget.exists {
//      pair => ((pair._1 union pair._2) -- vs).nonEmpty
//    }
  }

  /**
    * Use this when you know that the update can never fail, e.g. because it never merges any equivalence classes.
    *
    * For example, this is the case when we simply rename variables (without double capture) or when we drop names.
    *
    * @param f
    * @return
    */
  def unsafeUpdate(f: SubstitutionUpdate): VarConstraints = {
    // TODO: More efficient implementation of this where we skip the checks
    update(f).get
  }

  def update(f: SubstitutionUpdate): Option[VarConstraints] = {
    assert(isConsistent,
      s"Applying an update to an already inconsistent constraint set " + this)

    //val alloced: Set[Var] = fst.allocedVars ++ snd.allocedVars

    val updateMap = updateMapFromSubstitution(f)
    val updatedUsage = updatedUsageInfo(updateMap)
    updatedUsage foreach (u => logger.debug(s"Updated usage by $updateMap from $usage to $u"))
    updatedUsage flatMap (updateFromNewUsage(_, updateMap))
  }

  private def updateMapFromSubstitution(f: SubstitutionUpdate): Map[Set[Var], Set[Var]] = {
    var prevMap = Map.empty[Set[Var], Set[Var]]
    var currMap = classes.map(c => (c,c.flatMap(f))).toMap
    while (currMap != prevMap) {
      logger.debug("Curr map: " + currMap)
      prevMap = currMap
      currMap = for {
        (ks, vs) <- prevMap
      } yield (ks, vs ++ prevMap.filterKeys(_.intersect(vs).nonEmpty).values.toSet.flatten)
    }
    currMap
  }

  private def updateFromNewUsage(newUsage: VarUsageByLabel, updateMap: Map[Set[Var], Set[Var]]): Option[VarConstraints] = {
    assert(VarConstraints.hasDisjointEqualityClasses(newUsage), "Non-disjoint classes in usage " + newUsage)
    val ensuredAfterUpdate = VarConstraints.updateDiseqs(ensuredDiseqs, updateMap)
    val newEnsured = ensuredAfterUpdate ++ VarConstraints.diseqsImpliedByAllocation(newUsage)
    logger.trace(s"Updated $ensuredDiseqs via $ensuredAfterUpdate to $newEnsured")
    val newSpeculation = VarConstraints.updateDiseqs(speculativeDiseqs, updateMap) -- newEnsured
    // Note: Since the update does not contain the information whether it is speculative or not,
    // speculative equalities must be changed/removed explicitly, not implicitly via updates
    val res = VarConstraints(newUsage, newEnsured, newSpeculation, speculativeEqs)

    if (res.isInconsistent) {
      logger.debug(s"The update $updateMap has turned $this into inconsistent constraints $res")
      None
    }
    else {
      Some(res)
    }
  }

  // TODO It seems like we don't need this, since this is already achieved in the ++ operation?
//  def markAsEnsured(atoms: Set[PureAtom]): VarConstraints = {
//    val (eqs, diseqs) = atoms.partition(_.isEquality)
//    val ensuredEqs = eqs.map(_.ordered).map(atom => (atom.l, atom.r))
//    val newSpeculativeEqs = speculativeEqs -- ensuredEqs
//    val newSpeculativeDiseqs = speculativeDiseqs filterNot {
//      diseq => atoms.exists(atom => VarConstraints.impliesDiseq(diseq, atom.l, atom.r))
//    }
//    copy(speculativeDiseqs = newSpeculativeDiseqs, speculativeEqs = newSpeculativeEqs)
//  }

  def addToSpeculation(atoms: Iterable[PureAtom]): VarConstraints = {
    val (eqs, diseqs) = atoms.partition(_.isEquality)
    val newSpeculativeEqs = speculativeEqs ++ eqs.map(_.ordered).map(atom => (atom.l,atom.r))
    val newSpeculativeDiseqs = speculativeDiseqs filterNot {
      diseq => atoms.exists(atom => VarConstraints.impliesDiseq(diseq, atom.l, atom.r))
    }
    copy(speculativeDiseqs = newSpeculativeDiseqs, speculativeEqs = newSpeculativeEqs)
  }

  def addToSpeculationUnlessEnsured(atoms: Iterable[PureAtom]): VarConstraints = {
    if (atoms.nonEmpty) {
      val notImplied = atoms filterNot impliesWithoutSpeculation
      addToSpeculation(notImplied)
    } else this
  }

  private def updatedUsageInfo(updateMap: Map[Set[Var], Set[Var]]): Option[Map[Set[Var], VarUsage]] = {
    val grouped: Map[Set[Var], Map[Set[Var], VarUsage]] = usage.groupBy(pair => updateMap(pair._1))

    def doubleAlloc(group: Map[Set[Var], VarUsage]) : Boolean = group.values.count(_ == VarAllocated) >= 2

    if (grouped.values.exists(doubleAlloc)) {
      logger.debug(s"Update via $updateMap failed because of double allocation (${grouped.values.filter(doubleAlloc)})")
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

  def hasNamesForAllUsedParams: Boolean = {
    usage forall {
      case (vs, usage) => usage == VarUnused || vs.exists(!PlaceholderVar.isPlaceholder(_))
    }
  }

  override def toString: String = {
    val usageStr = usage.map{
      case (vs, usage) => vs.mkString(",") + ": " + usage.shortString
    }.mkString("usage = {", "; ", "}")

    val ensuredStr = if (ensuredDiseqs.nonEmpty) {
      ensuredDiseqs.mkString("; ensured = {", ",", "}")
    } else ""

    val missingDiseqsStrs = speculativeDiseqs map (pair => pair._1.mkString("{",",","}") + '\u2249' + pair._2.mkString("{",",","}") )
    val missingEqsStrs = speculativeEqs map (pair => PureAtom(pair._1, pair._2, isEquality = true).toString)
    val missingStrs = missingDiseqsStrs ++ missingEqsStrs
    val missingStr = if (missingStrs.nonEmpty) {
      missingStrs.mkString("; missing = {", ",", "}")
    } else ""

    "Constraints(" + usageStr + ensuredStr + missingStr + ")"
  }

}

object VarConstraints extends HarrshLogging {

  // TODO: Do we actually need this? It should be enough to explicitly add disequalities to the ensured constraints?
//  def dropIfBothAllocated(speculativeDiseqs: Set[(Set[Var], Set[Var])], newUsage: Map[Set[Var], VarUsage]): Set[(Set[Var], Set[Var])] = {
//    speculativeDiseqs filterNot {
//      // Note: The first condition ensures that we don't accidentally drop inconsistent constraints!
//      // TODO: Check if we actually need that condition, as we only apply this to missing constraints
//      pair => (pair._1 != pair._2) && newUsage(pair._1) == VarAllocated && newUsage(pair._2) == VarAllocated
//    }
//  }

  def fromAtoms(vars: Set[Var], atoms: Iterable[PureAtom]): VarConstraints = {
    val closure = Closure.ofAtoms(atoms)
    val closureClasses = closure.classes
    val trivialClasses = (vars -- closureClasses.flatten) map (Set(_))
    val allClasses = closureClasses ++ trivialClasses
    def classOf(v: Var) = allClasses.find(_.contains(v)).getOrElse{
      throw new IllegalArgumentException(s"Constructing constraints for $vars, but $atoms contain additional variable $v")
    }
    logger.debug(s"Classes of $atoms: ${allClasses.mkString(",")}")
    val usage: VarUsageByLabel = allClasses.zip(Stream.continually(VarUnused)).toMap
    val ensuredDiseqs = atoms.filter(!_.isEquality).map{
      case PureAtom(l, r, _) => (classOf(l), classOf(r))
    }
    VarConstraints(usage, ensuredDiseqs.toSet, Set.empty, Set.empty)
  }

  def dropRedundantPlaceholders(vs: Set[Var]): Set[Var] = {
    val (phs, nonphs) = vs.partition(PlaceholderVar.isPlaceholder)
    // Retain at most one placeholder per class -- and only if there are no proper names.
    if (nonphs.nonEmpty) nonphs else Set(phs.head)
  }

  // TODO: Can we manage to get around this, e.g. by storing *no* instead of *all* disequalities implied by allocation?
  def diseqsImpliedByAllocation(usage: VarUsageByLabel): Set[(Set[Var], Set[Var])] = {
    val allocedClasses = usage.filter(_._2 == VarAllocated).keySet
    Combinators.pairsWithoutRepetitions(allocedClasses)
  }

  def updateDiseqs(diseqs: Set[(Set[Var], Set[Var])], updateMap: Map[Set[Var], Set[Var]]): Set[(Set[Var], Set[Var])] = {
    diseqs map (pair => (updateMap(pair._1), updateMap(pair._2)))
  }

  def updateAndDropEmptyDiseqs(diseqs: Set[(Set[Var], Set[Var])], updateMap: Map[Set[Var], Set[Var]]): Set[(Set[Var], Set[Var])] = {
    updateDiseqs(diseqs, updateMap) filterNot (pair => pair._1.isEmpty || pair._2.isEmpty)
  }

  def impliesDiseq(diseq: (Set[Var], Set[Var]), l: Var, r: Var): Boolean = {
    Set(diseq, diseq.swap).exists(pair => pair._1.contains(l) && pair._2.contains(r))
  }

  def hasDisjointEqualityClasses(usage: VarUsageByLabel) = Combinators.counts(usage.keys.toSeq.flatten).forall(_._2 == 1)

}