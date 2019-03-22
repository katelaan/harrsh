package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.entailment.VarConstraints.{DiseqConstraint, PartitionedDiseqs, RewrittenBoundVarDiseqConstraint}
import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.pure.Closure
import at.forsyte.harrsh.seplog.inductive.PureAtom
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.util.Combinators

case class VarConstraints(usage: VarUsageByLabel, ensuredDiseqs: Set[DiseqConstraint], speculativeDiseqs: Set[DiseqConstraint], speculativeEqs: Set[(Var, Var)], rewrittenSpeculation: Set[RewrittenBoundVarDiseqConstraint]) extends HarrshLogging {
  // FIXME: Also need to try to express speculative eqs using non-dropped vars! (Which is only possible by passing the shared constraints, as we cannot otherwise reconstruct the underlying equality classes that could be used in rewriting.)

  assert(isWellFormed, s"$this is not well-formed")
  assert(allocatedEnsuredNotNull)
  assert(isConsistent)

  val isSpeculative: Boolean = speculativeDiseqs.nonEmpty || speculativeEqs.nonEmpty || rewrittenSpeculation.nonEmpty

  lazy val allVars: Set[Var] = usage.keySet.flatten
  lazy val allocedVars: Set[Var] = usage.filter(_._2 == VarAllocated).keySet.flatten
  lazy val boundVars: Set[Var] = allVars filter (_.isBound)
  lazy val nonNullNonPlaceholderVars: Set[Var] = allVars.filterNot(v => v.isNull || PlaceholderVar.isPlaceholder(v))
  lazy val placeholders: Set[Var] = allVars filter PlaceholderVar.isPlaceholder
  lazy val nullAlloced: Boolean = usageOfOption(NullConst).contains(VarAllocated)

  lazy val classes: Set[Set[Var]] = usage.keySet

  def redundantPlaceholders: Set[Var] = for {
    cls <- classes
    (phs, nonphs) = cls.partition(PlaceholderVar.isPlaceholder)
    if nonphs.nonEmpty
    ph <- phs
  } yield ph

  private lazy val allDiseqs: Set[DiseqConstraint] = ensuredDiseqs ++ speculativeDiseqs

  private def isWellFormed: Boolean = {
    val noOverlaps = VarConstraints.hasDisjointEqualityClasses(usage)
    val diseqsAmongClasses = allDiseqs forall (diseq => diseq.underlying subsetOf classes)
    val ensuredNotSpeculative = ensuredDiseqs.intersect(speculativeDiseqs).isEmpty
    val nonEmptyClasses = classes forall (_.nonEmpty)
    val orderedSpeculation = speculativeEqs forall (p => p._1 <= p._2)
    val speculativeEqsNotPlaceholder = speculativeEqs forall (pair => !PlaceholderVar.isPlaceholder(pair._1) && !PlaceholderVar.isPlaceholder(pair._2))
    val rewrittenAmongClasses = rewrittenSpeculation forall (_.allVarsAmong(allVars))
    logger.trace(s"No overlaps=$noOverlaps, diseqs among classes=$diseqsAmongClasses, ensured not speculative=$ensuredNotSpeculative, non-empty classes=$nonEmptyClasses, ordered speculation=$orderedSpeculation, no placeholder speculation=$speculativeEqsNotPlaceholder, rewritten")
    noOverlaps && diseqsAmongClasses && ensuredNotSpeculative && nonEmptyClasses && orderedSpeculation && speculativeEqsNotPlaceholder && rewrittenAmongClasses
  }

  private def allocatedEnsuredNotNull: Boolean = {
    VarConstraints.diseqsImpliedByAllocation(usage) subsetOf ensuredDiseqs
  }

  private def isConsistent: Boolean = {
    def contradictoryDiseq = allDiseqs.exists(_.isContradictory)
    !nullAlloced && !contradictoryDiseq
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

  def restrictToNonPlaceholdersAnd(classesToKeep: Set[Set[Var]]): Option[VarConstraints] = {
    val varsToRetainExplicitly = classesToKeep.flatten
    val varsToRetain = allVars filter (v => varsToRetainExplicitly(v) || !PlaceholderVar.isPlaceholder(v))
    val varsToDrop = allVars -- varsToRetain

    val PartitionedDiseqs(unaffected, newRewritten, nonrewritable) = VarConstraints.splitSpeculativeDiseqsOnDroppedVars(this, varsToDrop)
    // FIXME: Should also check if speculative eqs are affected?
    if (nonrewritable.nonEmpty) {
      logger.debug(s"Can't restrict $this\n to $classesToKeep,\nbecause that would lose speculative disequalities $nonrewritable")
      None
    } else {
      Some(VarConstraints(
        usage.filterKeys(_.exists(varsToRetain)),
        ensuredDiseqs.filter(_.isAbout(varsToRetain)),
        unaffected,
        speculativeEqs,
        rewrittenSpeculation ++ newRewritten
      ))
    }
  }

  def restrictPlaceholdersTo(placeholders: Set[Var]): Option[VarConstraints] = {
    val allPlaceholders = allVars filter PlaceholderVar.isPlaceholder
    val toDrop = allPlaceholders -- placeholders
    val dropper = DropperUpdate(toDrop)
    dropper(this)
  }

  def speculativeEqsNotEnsuredIn(other: VarConstraints): Set[(Var, Var)] = {
    val res = speculativeEqs.filterNot(atom => other.impliesWithoutSpeculation(PureAtom(atom._1, atom._2, isEquality = true)))
    logger.trace(s"Checking which among $speculativeEqs are not ensured in $other => $res")
    res
  }

  /**
    * Returns true iff forgetting vs would lose one of the speculative constraints.
    *
    * This would be a source of unsoundness, since we'd forget that the context decomposition is only possible under
    * the assumption that somewhere in the call context we will establish the disequality.
    * (Putting one or missing constraints out of scope means the entailment can never become true.)
    */
  def areRequiredInSpeculation(vs: Set[Var]): Boolean = {
    areRequiredInSpeculativeEqs(vs) || areRequiredInSpeculativeDiseqs(vs)
  }

  private def areRequiredInSpeculativeEqs(vs: Set[Var]): Boolean = {
    speculativeEqs exists {
      pair => vs.contains(pair._1) || vs.contains(pair._2)
    }
  }

  private def areRequiredInSpeculativeDiseqs(vs: Set[Var]): Boolean = {
    speculativeDiseqs exists (_.requires(vs, this))
  }

  def hasNamesForAllUsedParams: Boolean = {
    usage forall {
      case (vs, theUsage) => theUsage == VarUnused || vs.exists(!PlaceholderVar.isPlaceholder(_))
    }
  }

  override def toString: String = {
    val usageStr = usage.map{
      case (vs, theUsage) => vs.mkString(",") + ": " + theUsage.shortString
    }.mkString("usage = {", "; ", "}")

    val ensuredStr = if (ensuredDiseqs.nonEmpty) {
      ensuredDiseqs.mkString("; ensured = {", ",", "}")
    } else ""

    val missingDiseqsStrs = speculativeDiseqs map (_.toString)
    val missingEqsStrs = speculativeEqs map (pair => PureAtom(pair._1, pair._2, isEquality = true).toString)
    val rewrittenStrs = rewrittenSpeculation map (_.toString)
    val missingStrs = missingDiseqsStrs ++ rewrittenStrs ++ missingEqsStrs
    val missingStr = if (missingStrs.nonEmpty) {
      missingStrs.mkString("; missing = {", ",", "}")
    } else ""

    "Constraints(" + usageStr + ensuredStr + missingStr + ")"
  }

}

object VarConstraints extends HarrshLogging {

  case class DiseqConstraint(underlying: Set[Set[Var]]) {

    assert(Set(1,2).contains(underlying.size))

    lazy val toPair = (underlying.head, if (underlying.size == 2) underlying.tail.head else underlying.head)

    def isAbout(vars: Set[Var]): Boolean = underlying forall (_.exists(vars))

    def isContradictory: Boolean = underlying.size == 1

    def needsPlaceholder: Boolean = underlying.exists {
      _ forall PlaceholderVar.isPlaceholder
    }

    def isVacuous: Boolean = underlying.exists(_.isEmpty)

    def update(f: ConstraintUpdater) = DiseqConstraint(underlying.map(f(_)))

    def wouldBeDroppedWithRemovalOf(vs: Set[Var]): Boolean = underlying exists (_ subsetOf vs)

    def requires(vs: Set[Var], container: VarConstraints): Boolean = {
      val (droppedSides,keptSides) = underlying partition (_ subsetOf vs)
      droppedSides.size match {
        case 0 => false
        case 1 if keptSides.isEmpty => true
        case 1 => !canExpressWithout(vs, droppedSides.head, keptSides.head, container)
        case i =>
          assert(i == 2)
          true
      }
    }

    private def containsOnly(vars: Set[Var]): Boolean = underlying forall (_ subsetOf vars)

    private def canExpressWithout(varsToDrop: Set[Var], droppedSide: Set[Var], keptSide: Set[Var], container: VarConstraints): Boolean = {
      val keptSideIsAlloced = container.usage(keptSide) == VarAllocated
      val res = if (keptSideIsAlloced) {
        // Can create a speculative-allocation constraint that would imply the disequality
        true
      } else {
        // Is there an ensured disequality mentioning the dropped side? In that case, the constraint will be implied if we can later prove
        // an equality among the other argument of the ensured disequality and the kept side
        container.ensuredDiseqs.exists(diseq => diseq.underlying.contains(droppedSide) && !diseq.containsOnly(varsToDrop))
      }
      logger.debug(s"Checking whether $droppedSide != $keptSide (dropped together with $varsToDrop) can be expressed differently wrt. $container: $res")
      res
    }

    def expressWithout(vs: Set[Var], container: VarConstraints): Option[RewrittenBoundVarDiseqConstraint] = {
      // TODO: Reduce code duplication with requires
      val (droppedSides,keptSides) = underlying partition (_ subsetOf vs)
      droppedSides.size match {
        case 0 => throw new IllegalArgumentException("This constraint need not be rewritten")
        case 1 if keptSides.isEmpty => None
        case 1 => expressWithout(vs, droppedSides.head, keptSides.head, container)
        case i =>
          assert(i == 2)
          None
      }
    }

    private def expressWithout(varsToDrop: Set[Var], droppedSide: Set[Var], keptSide: Set[Var], container: VarConstraints): Option[RewrittenBoundVarDiseqConstraint] = {
      val keptSideIsAlloced = container.usage(keptSide) == VarAllocated

      // Collect ensured disequalities mentioning the dropped side. In that case, the constraint will be implied if we can later prove
      // an equality among the other argument of an ensured disequality and the kept side
      val ensuredDiseqsWithDroppedVar = container.ensuredDiseqs.filter(diseq => diseq.underlying.contains(droppedSide) && !diseq.containsOnly(varsToDrop))
      val equalitiesToProve = ensuredDiseqsWithDroppedVar.flatMap(_.underlying) - droppedSide

      if (keptSideIsAlloced || equalitiesToProve.nonEmpty) {
        Some(RewrittenBoundVarDiseqConstraint(keptSide, equalitiesToProve, isAllocOrNullSufficient = keptSideIsAlloced))
      } else {
        logger.debug(s"It's impossible to express $droppedSide != $keptSide without $varsToDrop => Will discard constraints")
        None
      }
    }

    def isImpliedBy(l: Var, r: Var): Boolean = {
      Set(toPair, toPair.swap).exists(pair => pair._1.contains(l) && pair._2.contains(r))
    }

    override def toString: String = {
      val (fst, snd) = toPair
      fst.mkString("{",",","}") + '\u2249' + snd.mkString("{",",","}")
    }
  }

  /**
    * When bound variabled involved in speculative diseqs go out of scope, we have to rewrite the constraints to (generally stronger) constraints
    * that do not mention the placeholder, but that imply that the disequality holds.
    *
    * For example, given a speculative disequality alpha1 != ?1 and the (ensured) constraints alpha1: alloced and alpha1 != x2, we should compute the constraint
    * RewrittenBoundVarDiseqConstraint(Set(?1), Set(x2), isAllocOrNullSufficient = true) to indicate that the disequality x1 != ?1 can be made true either by
    * (1) Proving that ?1 is allocated or null (since there is no double alloc/null allocation and alpha1 is alloced, alpha1 != ?1 is then guaranteed); or
    * (2) Proving that ?1 = x2, because the disequality alpha1 != x2 is already known and thus alpha1 != ?1 follows from the equality.
    *
    * Note: If no rewriting is possible,
    *
    * @param remainingDiseqSide The side of the disequality not involivng the forgotten bound var
    * @param classesDiseqFromLostVar The underlying constraint follows if remainingDiseqSide is unified with any var among disequalFromLostVar
    * @param isAllocOrNullSufficient Can the underlying constraint be fulfilled by unifying with an allocated variable?
    */
  case class RewrittenBoundVarDiseqConstraint(remainingDiseqSide: Set[Var], classesDiseqFromLostVar: Set[Set[Var]], isAllocOrNullSufficient: Boolean) {

    def allVarsAmong(vars: Set[Var]): Boolean = (remainingDiseqSide subsetOf vars) && classesDiseqFromLostVar.forall(_ subsetOf vars)

    def update(f: ConstraintUpdater) = RewrittenBoundVarDiseqConstraint(
      f(remainingDiseqSide), classesDiseqFromLostVar.map(f.apply).filterNot(_.isEmpty), isAllocOrNullSufficient
    )

    def isImpliedBy(usage: VarUsageByLabel): Boolean = {
      if (isAllocOrNullSufficient && (usage(remainingDiseqSide) == VarAllocated || remainingDiseqSide.contains(NullConst))) {
        true
      } else {
        // When the classes of the diseq side and some other diseq sid become equal through an update,
        // this will make the sets of vars stored here equal as well
        classesDiseqFromLostVar.contains(remainingDiseqSide)
      }
    }

    def isUnsat: Boolean = {
      remainingDiseqSide.isEmpty || (!isAllocOrNullSufficient && classesDiseqFromLostVar.isEmpty)
    }

    override def toString: String = {
      val allocStr = if (isAllocOrNullSufficient) s"alloced_or_null(${remainingDiseqSide.mkString(",")})" else ""
      val eqStrs = classesDiseqFromLostVar map {
        diseq => remainingDiseqSide.mkString("{",",","}") + '\u2248' + diseq.mkString("{",",","}")
      }
      val allStrs = (allocStr +: eqStrs.toSeq).filterNot(_.isEmpty)
      if (allStrs.size > 1) {
        allStrs.mkString("[", "\u2228", "]")
      } else {
        allStrs.head
      }
    }

  }

  def updateRewrittenConstraints(f: ConstraintUpdater, newUsage: VarUsageByLabel, rewritten: Set[RewrittenBoundVarDiseqConstraint]): Option[Set[RewrittenBoundVarDiseqConstraint]] = {
    val updated = rewritten map (_.update(f)) filterNot (_.isImpliedBy(newUsage))
    if (updated.exists(_.isUnsat)) {
      logger.debug("After an update, a rewritten constraint has become unsatisfiable")
      None
    } else {
      Some(updated)
    }
  }

  case class PartitionedDiseqs(unaffected: Set[DiseqConstraint], rewritten: Set[RewrittenBoundVarDiseqConstraint], nonrewritable: Set[DiseqConstraint])

  def splitSpeculativeDiseqsOnDroppedVars(constraints: VarConstraints, varsToDrop: Set[Var]): PartitionedDiseqs = {
    val diseqs = constraints.speculativeDiseqs
    val (wouldBeDropped, notDropped) = diseqs.partition(_.wouldBeDroppedWithRemovalOf(varsToDrop))
    val withRewriting = wouldBeDropped.map(diseq => (diseq, diseq.expressWithout(varsToDrop, constraints)))
    val (nonrewritablePairs, rewrittenPairs) = withRewriting.partition(_._2.isEmpty)
    val nonrewritable = nonrewritablePairs map (_._1)
    if (rewrittenPairs.nonEmpty) {
      logger.debug(s"Rewrote some disequalities to avoid using $varsToDrop:\n" + rewrittenPairs.map(pair => s"${pair._1} => ${pair._2.get}").mkString(", "))
    }
    val rewritten = rewrittenPairs map (_._2.get)
    PartitionedDiseqs(notDropped, rewritten, nonrewritable)
  }

  def fromAtoms(vars: Iterable[Var], atoms: Iterable[PureAtom]): VarConstraints = {
    val closure = Closure.ofAtoms(atoms)
    val closureClasses = closure.classes
    val trivialClasses = ((vars.toSet + NullConst) -- closureClasses.flatten) map (Set(_))
    val allClasses = closureClasses ++ trivialClasses
    def classOf(v: Var) = allClasses.find(_.contains(v)).getOrElse{
      throw new IllegalArgumentException(s"Constructing constraints for $vars, but $atoms contain additional variable $v")
    }
    logger.debug(s"Classes of $atoms: ${allClasses.mkString(",")}")
    val usage: VarUsageByLabel = allClasses.zip(Stream.continually(VarUnused)).toMap
    val ensuredDiseqs = atoms.filter(!_.isEquality).map{
      case PureAtom(l, r, _) => DiseqConstraint(Set(classOf(l), classOf(r)))
    }
    VarConstraints(usage, ensuredDiseqs.toSet, Set.empty, Set.empty, Set.empty)
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

  def hasDisjointEqualityClasses(usage: VarUsageByLabel): Boolean = Combinators.counts(usage.keys.toSeq.flatten).forall(_._2 == 1)

  def combineEnsured(css: Seq[VarConstraints]): Option[VarConstraints] = {
    val res = if (css.tail.isEmpty) {
      Some(css.head)
    } else {
      val fst = css.head
      val snd = css.tail.head
      val rest = css.tail.tail
      for {
        combined <- MergeUpdate.mergeWithUnifyingUpdate(fst, snd)
        restMerged <- combineEnsured(combined +: rest)
      } yield restMerged
    }
    res match {
      case None => logger.debug("Could not combine constraints:\n" + css.mkString("\n"))
      case Some(cs) => logger.debug(s"Combined constraints:\n${css.mkString("\n")}\ninto $cs")
    }
    res
  }

  def areCompatible(profileConstraints: VarConstraints, topLevelConstraints: Iterable[PureAtom]): Boolean = {
    val (eqs, diseqs) = topLevelConstraints.partition(_.isEquality)
    val diseqSets = diseqs map (atom => Set(atom.l, atom.r))
    profileConstraints.ensuredDiseqs.map(_.toPair).forall{
      !contradictedBy(_, eqs)
    } && profileConstraints.classes.forall{
      !contradictedBy(_, diseqSets)
    }
  }

  private def contradictedBy(constraint: (Set[Var], Set[Var]), eqs: Iterable[PureAtom]): Boolean = {
    eqs.exists{
      atom => (constraint._1.contains(atom.l) && constraint._2.contains(atom.r)) || (constraint._1.contains(atom.r) && constraint._2.contains(atom.l))
    }
  }

  private def contradictedBy(cls: Set[Var], diseqs: Iterable[Set[Var]]): Boolean = {
    // If both arguments of a disequality are contained in the equivalence class, that's a contradiction
    diseqs.exists(_ subsetOf cls)
  }

//  private def notEqualInClosure(constraint: DiseqConstraint, closure: Closure): Boolean = {
//    def getClass(vs: Set[Var]): Set[Var] = vs.flatMap(closure.getEquivalenceClass(_))
//    getClass(constraint.underlying.head).intersect(getClass(constraint.underlying.tail.head)).isEmpty
//  }

}