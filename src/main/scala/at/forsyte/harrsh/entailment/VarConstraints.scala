package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.VarConstraints.DiseqConstraint
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.util.Combinators

case class VarConstraints(usage: VarUsageByLabel, ensuredDiseqs: Set[DiseqConstraint], speculativeDiseqs: Set[DiseqConstraint], speculativeEqs: Set[(Var, Var)]) extends HarrshLogging {

  // TODO: The usage map / classes implicitly contain speculation (the result of identifying parameters in matching). Do we handle this correctly? Think in particular about going to the sink state or returning no state in profile composition when we filter out all decompositions because of inconsistencies
  // FIXME: It seems like the implicit equality speculation is actually unsound in case we forget one of the participating variables! Should probably track such equalities explicitly here as well!

  assert(isWellFormed, s"$this is not well-formed")
  assert(VarConstraints.diseqsImpliedByAllocation(usage) subsetOf ensuredDiseqs)

  val isSpeculative: Boolean = speculativeDiseqs.nonEmpty || speculativeEqs.nonEmpty

  lazy val allVars: Set[Var] = usage.keySet.flatten
  lazy val allocedVars: Set[Var] = usage.filter(_._2 == VarAllocated).keySet.flatten
  lazy val boundVars: Set[Var] = allVars filter (_.isBound)
  lazy val nonNullNonPlaceholderVars: Set[Var] = allVars.filterNot(v => v.isNull || PlaceholderVar.isPlaceholder(v))
  lazy val placeholders: Set[PlaceholderVar] = allVars flatMap PlaceholderVar.fromVar
  lazy val nullAlloced: Boolean = usageOfOption(NullConst).contains(VarAllocated)

  lazy val classes: Set[Set[Var]] = usage.keySet

  private lazy val allDiseqs: Set[DiseqConstraint] = ensuredDiseqs ++ speculativeDiseqs

  def isWellFormed: Boolean = {
    val noOverlaps = VarConstraints.hasDisjointEqualityClasses(usage)
    val diseqsAmongClasses = allDiseqs forall (diseq => diseq.underlying subsetOf classes)
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
    if (atom.isEquality) {
      val lclass = classOfOption(atom.l)
      lclass.nonEmpty && lclass == classOfOption(atom.r)
    } else {
      ensuredDiseqs exists (_.isImpliedBy(atom.l, atom.r))
    }
  }

  def isConsistent: Boolean = {
    def contradictoryDiseq = allDiseqs.exists(_.isContradictory)
    !nullAlloced && !contradictoryDiseq
  }

  def isInconsistent: Boolean = !isConsistent

  def forget(vs: Set[Var]): VarConstraints = {
    assume(!areRequiredInSpeculation(vs))

    val f = SubstitutionUpdate.forgetVars(vs)
    val newUsage = usage.map(pair => (f(pair._1), pair._2)).filterKeys(_.nonEmpty)
    val newEnsuredDiseqs = VarConstraints.updateAndDropEmptyDiseqs(ensuredDiseqs, f)
    //val newSpeculation = VarConstraints.updateAndDropEmptyDiseqs(speculatedDiseqs, updateMap)
    // Because we're never allowed to lose speculative constraints in a forget operation, we don't need to check for empty constraints here.
    val newSpeculativeDiseqs = VarConstraints.updateDiseqs(speculativeDiseqs, f)
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
        ensuredDiseqs.filter(_.isAbout(varsToRetain)),
        speculativeDiseqs,
        speculativeEqs
      ))
    }
  }

  def mergeUsingUpdate(otherBeforeUpdate: VarConstraints, upd: SubstitutionUpdate): Option[VarConstraints] = {
    logger.debug(s"Will try to merge constraints:\n$this and\n$otherBeforeUpdate")
    for {
      thisUpd <- update(upd, mayEnsureEqualities = true)
      other <- otherBeforeUpdate.update(upd, mayEnsureEqualities = true)
      _ = assume(thisUpd.allocedVars.intersect(other.allocedVars).isEmpty)
      allClasses = thisUpd.classes ++ other.classes
      newUsage = allClasses.map(c => (c, Set(thisUpd.usage.getOrElse(c,VarUnused), other.usage.getOrElse(c,VarUnused)).max)).toMap
      allEnsured = thisUpd.ensuredDiseqs ++ other.ensuredDiseqs ++ VarConstraints.diseqsImpliedByAllocation(newUsage)
      // Note: Crucially, we check whether the missing equalities are ensured *before* the update
      // After the update, they are ensured by definition, as they will have been propagated into the classes!
      missingEqs = speculativeEqsNotEnsuredIn(otherBeforeUpdate) ++ otherBeforeUpdate.speculativeEqsNotEnsuredIn(this)
    } yield VarConstraints(
      newUsage,
      allEnsured,
      (speculativeDiseqs ++ other.speculativeDiseqs) -- allEnsured,
      missingEqs
    )
  }

  def speculativeEqsNotEnsuredIn(other: VarConstraints): Set[(Var, Var)] = {
    val res = speculativeEqs.filterNot(atom => other.impliesWithoutSpeculation(PureAtom(atom._1, atom._2, isEquality = true)))
    logger.debug(s"Checking which among $speculativeEqs are not ensured in $other => $res")
    res
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
    speculativeDiseqs exists (_.requires(vs))
  }

  /**
    * Use this when you know that the update can never fail, e.g. because it never merges any equivalence classes.
    *
    * For example, this is the case when we simply rename variables (without double capture) or when we drop names.
    *
    * @param f
    * @return
    */
  def unsafeUpdate(f: SubstitutionUpdate, mayEnsureEqualities: Boolean): VarConstraints = {
    // TODO: More efficient implementation of this where we skip the checks
    update(f, mayEnsureEqualities).get
  }

  def update(f: SubstitutionUpdate, mayEnsureEqualities: Boolean): Option[VarConstraints] = {
    assert(isConsistent,
      s"Applying an update to an already inconsistent constraint set " + this)
    val updatedUsage = updatedUsageInfo(f)
    updatedUsage foreach (u => logger.debug(s"Updated usage by $f from $usage to $u"))
    updatedUsage flatMap (updateFromNewUsage(_, f, mayEnsureEqualities))
  }

  private def updateFromNewUsage(newUsage: VarUsageByLabel, f: SubstitutionUpdate, mayEnsureEqualities: Boolean): Option[VarConstraints] = {
    assert(VarConstraints.hasDisjointEqualityClasses(newUsage), "Non-disjoint classes in usage " + newUsage)
    val ensuredAfterUpdate = VarConstraints.updateDiseqs(ensuredDiseqs, f)
    val newEnsured = ensuredAfterUpdate ++ VarConstraints.diseqsImpliedByAllocation(newUsage)
    logger.trace(s"Updated $ensuredDiseqs via $ensuredAfterUpdate to $newEnsured")
    val newSpeculativeDiseqs = VarConstraints.updateDiseqs(speculativeDiseqs, f) -- newEnsured
    val newSpeculativeEqs = updateSpeculativeEqs(f, mayEnsureEqualities)
    val res = VarConstraints(newUsage, newEnsured, newSpeculativeDiseqs, newSpeculativeEqs)

    if (res.isInconsistent) {
      logger.debug(s"The update $f has turned $this into inconsistent constraints $res")
      None
    }
    else {
      Some(res)
    }
  }

  private def updateSpeculativeEqs(f: SubstitutionUpdate, mayEnsureEqualities: Boolean): Set[(Var,Var)] = {
    if (mayEnsureEqualities) {
      speculativeEqs.map {
        eq => (f(eq._1).head, f(eq._2).head)
      }.filterNot(pair => pair._1 == pair._2)
    } else {
      speculativeEqs map {
        // If the update sets equal the variables, we must not execute it, sicne we're not allowed to propagate this information
        // This is an extremely hacky solution...
        eq => if (Set(eq._1,eq._2) subsetOf f(eq._1)) eq else (f(eq._1).head,f(eq._2).head)
      }
    }
  }

  def addToSpeculation(atoms: Iterable[PureAtom]): Option[VarConstraints] = {
    val (eqs, diseqs) = atoms.partition(_.isEquality)

    // Make sure the speculative equalities are reflected in the equivalence classes
    val maybeUpdated = if (eqs.nonEmpty) {
      val f = SubstitutionUpdate.fromSetsOfEqualVars(eqs.map(_.getVars)).closeUnderEquivalenceClasses(classes)
      logger.debug(s"Speculative equalities must be propagated into $this via $f")
      update(f, mayEnsureEqualities = false)
    } else {
      Some(this)
    }

    maybeUpdated match {
      case Some(updated) =>
        val newSpeculativeEqs = speculativeEqs ++ eqs.map(_.ordered).map(atom => (atom.l,atom.r))
        // The speculative equalities may invalidate some of the speculative disequalities
        // We thus remove the now-ensured equalities from the speculative disequalities
        val nowEnsured = updated.ensuredDiseqs
        val newSpeculativeDiseqs = (speculativeDiseqs ++ diseqs.map{
          case PureAtom(l, r, _) => DiseqConstraint(Set(updated.classOf(l), updated.classOf(r)))
        }) -- nowEnsured
        logger.debug(s"Considering $atoms for speculation wrt $this:\nNow have speculative equalities $newSpeculativeEqs and disequalities $newSpeculativeDiseqs.")

        Some(updated.copy(speculativeDiseqs = newSpeculativeDiseqs, speculativeEqs = newSpeculativeEqs))
      case None =>
        logger.debug(s"Speculation $atoms would lead to double allocation. Returning no result.")
        None
    }
  }

  def addToSpeculationUnlessEnsured(atoms: Iterable[PureAtom]): Option[VarConstraints] = {
    if (atoms.nonEmpty) {
      val notImplied = atoms filterNot impliesWithoutSpeculation
      addToSpeculation(notImplied)
    } else Some(this)
  }

  private def updatedUsageInfo(f: SubstitutionUpdate): Option[Map[Set[Var], VarUsage]] = {
    val grouped: Map[Set[Var], Map[Set[Var], VarUsage]] = usage.groupBy(pair => f(pair._1))

    def doubleAlloc(group: Map[Set[Var], VarUsage]) : Boolean = group.values.count(_ == VarAllocated) >= 2

    if (grouped.values.exists(doubleAlloc)) {
      logger.debug(s"Update via $f failed because of double allocation (${grouped.values.filter(doubleAlloc)})")
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

    val missingDiseqsStrs = speculativeDiseqs map (_.toString)
    val missingEqsStrs = speculativeEqs map (pair => PureAtom(pair._1, pair._2, isEquality = true).toString)
    val missingStrs = missingDiseqsStrs ++ missingEqsStrs
    val missingStr = if (missingStrs.nonEmpty) {
      missingStrs.mkString("; missing = {", ",", "}")
    } else ""

    "Constraints(" + usageStr + ensuredStr + missingStr + ")"
  }

}

object VarConstraints extends HarrshLogging {

  case class DiseqConstraint(underlying: Set[Set[Var]]) {

    assert(Set(1,2).contains(underlying.size))

    private lazy val toPair = (underlying.head, if (underlying.size == 2) underlying.tail.head else underlying.head)

    def isAbout(vars: Set[Var]): Boolean = underlying forall (_.exists(vars))

    def isContradictory: Boolean = underlying.size == 1

    def isVacuous: Boolean = underlying.exists(_.isEmpty)

    def update(f: SubstitutionUpdate) = DiseqConstraint(underlying.map(f(_)))

    def requires(vs: Set[Var]): Boolean = {
      underlying exists (_ subsetOf vs)
    }

    def isImpliedBy(l: Var, r: Var): Boolean = {
      Set(toPair, toPair.swap).exists(pair => pair._1.contains(l) && pair._2.contains(r))
    }

    override def toString: String = {
      val (fst, snd) = toPair
      fst.mkString("{",",","}") + '\u2249' + snd.mkString("{",",","}")
    }
  }

  def fromAtoms(vars: Iterable[Var], atoms: Iterable[PureAtom]): VarConstraints = {
    val closure = Closure.ofAtoms(atoms)
    val closureClasses = closure.classes
    val trivialClasses = (vars.toSet -- closureClasses.flatten) map (Set(_))
    val allClasses = closureClasses ++ trivialClasses
    def classOf(v: Var) = allClasses.find(_.contains(v)).getOrElse{
      throw new IllegalArgumentException(s"Constructing constraints for $vars, but $atoms contain additional variable $v")
    }
    logger.debug(s"Classes of $atoms: ${allClasses.mkString(",")}")
    val usage: VarUsageByLabel = allClasses.zip(Stream.continually(VarUnused)).toMap
    val ensuredDiseqs = atoms.filter(!_.isEquality).map{
      case PureAtom(l, r, _) => DiseqConstraint(Set(classOf(l), classOf(r)))
    }
    VarConstraints(usage, ensuredDiseqs.toSet, Set.empty, Set.empty)
  }

  def dropRedundantPlaceholders(vs: Set[Var]): Set[Var] = {
    val (phs, nonphs) = vs.partition(PlaceholderVar.isPlaceholder)
    // Retain at most one placeholder per class -- and only if there are no proper names.
    if (nonphs.nonEmpty) nonphs else Set(phs.head)
  }

  // TODO: Can we manage to get around this, e.g. by storing *no* instead of *all* disequalities implied by allocation?
  def diseqsImpliedByAllocation(usage: VarUsageByLabel): Set[DiseqConstraint] = {
    val allocedClasses = usage.filter(_._2 == VarAllocated).keySet
    Combinators.pairsWithoutRepetitions(allocedClasses) map {
      pair => DiseqConstraint(Set(pair._1, pair._2))
    }
  }

  def updateDiseqs(diseqs: Set[DiseqConstraint], f: SubstitutionUpdate): Set[DiseqConstraint] = {
    diseqs map (_.update(f))
  }

  def updateAndDropEmptyDiseqs(diseqs: Set[DiseqConstraint], f: SubstitutionUpdate): Set[DiseqConstraint] = {
    updateDiseqs(diseqs, f) filterNot (_.isVacuous)
  }

  def hasDisjointEqualityClasses(usage: VarUsageByLabel): Boolean = Combinators.counts(usage.keys.toSeq.flatten).forall(_._2 == 1)

}