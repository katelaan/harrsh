package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RichSid}

case class ContextDecomposition(parts: Set[EntailmentContext], constraints: VarConstraints) extends HarrshLogging {
  assert(constraints.isWellFormed, "Well-formedness constraint violated in " + constraints)

  assert(constraints.definedOnAllOf(occurringLabels),
    s"Inconsistent decomposition: Occurring labels are $occurringLabels, but constraints $constraints only defined on ${constraints.classes}"
  )

  assert(constraints.placeholders.map(_.toFreeVar.asInstanceOf[Var]) subsetOf occurringLabels.flatten,
    s"Decompositions in $this use only placeholders ${occurringLabels.flatten.filter(PlaceholderVar.isPlaceholder)}, but constraints refer to additional placeholders: ${constraints.placeholders}"
  )

  lazy val occurringLabels: Set[Set[Var]] = allPredCalls.flatMap(_.subst.toSeq)

  /**
    * A deterministic ordering of the parts (necessary to compute a deterministic normalform)
    */
  lazy val orderedParts: Seq[EntailmentContext] = parts.toSeq.sorted

  def isMultiPartDecomposition: Boolean = parts.size > 1

  private def allPredCalls: Set[ContextPredCall] = parts.flatMap(_.labels)

  private def orderedPredCalls: Seq[ContextPredCall] = orderedParts.flatMap(_.labels)

  // BEGIN Var-related code

  lazy val nonNullNonPlaceholderVars: Set[Var] = constraints.nonNullNonPlaceholderVars

  lazy val boundVars: Set[Var] = constraints.boundVars

  lazy val placeholders: Set[PlaceholderVar] = constraints.placeholders

  lazy val allocedVars: Set[Var] = constraints.allocedVars

  // END Var-related code

  // BEGIN compose, rename, forget

  def compositionOptions(sid: RichSid, other: ContextDecomposition): Seq[ContextDecomposition] = {
    for {
      composed <- ContextDecompositionComposition(sid, this, other)
      if composed.hasConsistentConstraints
      // Crucially, *DO NOT* check for consistency with focus here, as leaves will null alloc could still be removed in emp closure
    } yield composed
  }

  def updateSubst(f: SubstitutionUpdate, mayEnsureEqualities: Boolean): Option[ContextDecomposition] = {
    logger.debug(s"Will apply update $f to $this")
    val extendedF = f.closeUnderEquivalenceClasses(constraints.classes)
    constraints.update(extendedF, mayEnsureEqualities) map {
      updatedConstraints => ContextDecomposition(parts map (_.updateSubst(extendedF)), updatedConstraints)
    }
  }

  private def renameVarsToFreshPlaceholders(varsToRename: Iterable[Var]): SubstitutionUpdate = {
    val maxPlaceholder = PlaceholderVar.maxIndex(placeholders)
    val newPlaceholders = (1 to varsToRename.size) map (i => PlaceholderVar(maxPlaceholder + i))
    val pairs: Seq[(Var, Var)] = varsToRename.toSeq.zip(newPlaceholders.map(_.toFreeVar))
    SubstitutionUpdate.renaming(pairs)
  }

  def forget(varsToForget: Set[Var]): Option[ContextDecomposition] = {
    if (varsToForget.isEmpty) {
      Some(this)
    } else {
      logger.debug(s"Will remove variables ${varsToForget.mkString(",")} from decomposition")
      if (constraints.areRequiredInSpeculation(varsToForget)) {
        // We're forgetting variables for which there are still missing constraints
        // After dropping, it will no longer be possible to supply the constraints
        // We hence discard the decomposition
        logger.debug(s"Discarding decomposition: Speculative constraints in $constraints require at least one discarded variable from ${varsToForget.mkString(", ")}")
        None
      } else {
        Some(replaceByPlaceholders(varsToForget))
      }
    }
  }

  private def replaceByPlaceholders(vars: Set[Var]) = {
    // TODO: More efficient variable dropping for decomps?
    val update: SubstitutionUpdate = renameVarsToFreshPlaceholders(vars)
    val partsAfterDropping = parts map (_.updateSubst(update))
    val constraintsAfterDropping = constraints.unsafeUpdate(update, mayEnsureEqualities = true)
    val redundantPlaceholders = constraintsAfterDropping.placeholders filterNot {
      ph => partsAfterDropping.exists(_.placeholders.contains(ph))
    }
    logger.debug(s"Will drop redundant placeholders $redundantPlaceholders introduced when forgetting $vars")
    val cleanedConstraints = constraintsAfterDropping.forget(redundantPlaceholders.map(_.toFreeVar))

    ContextDecomposition(partsAfterDropping, cleanedConstraints).toPlaceholderNormalForm
  }

  // END compose, rename, forget

  def isFinal(sid: RichSid, rhs: TopLevelConstraint): Boolean = {
    val res = if (constraints.isSpeculative) {
      logger.debug(s"$constraints are speculative => Decomposition is non-final")
      false
    } else if (parts.size > rhs.size) {
      false
    } else {
      val lhsRoots = parts map (_.root)
      parts.forall(_.isConcrete) && rhs.isImpliedBy(lhsRoots, constraints, sid.predsWithEmptyModels)
    }
    logger.trace(s"Checking whether $this is final w.r.t. $rhs => $res")
    res
  }

  // BEGIN Consistency-related code

  def hasNamesForAllUsedParams: Boolean = constraints.hasNamesForAllUsedParams

  def hasNonNullNamesForAllRootParams(sid: RichSid): Boolean = parts.forall(_.hasNonNullNamesForRootParams(sid))

  def toPlaceholderNormalForm: ContextDecomposition = {
    // TODO Perhaps get rid of the first step, see assertion at the beginning of the class
    val withoutRedundancies = dropRedundantPlaceholders
    val orderedLabels = withoutRedundancies.orderedParts flatMap (_.labels)
    val establishNormalForm = ContextPredCall.placeholderNormalFormUpdater(orderedLabels)
    // Can never fail since we simply rename things without identifying them, so _.get is safe
    // Note: It does not matter for correctness what we pass to mayEnsureEqualities here, since we apply a bijection
    withoutRedundancies.updateSubst(establishNormalForm, mayEnsureEqualities = false).get
  }

  private def dropRedundantPlaceholders: ContextDecomposition = {
    val dropper = SubstitutionUpdate.redundantPlaceholderDropper(allPredCalls)
    val partsAfterDropping = parts map (_.updateSubst(dropper))
    val constraintsAfterDropping = constraints.unsafeUpdate(dropper, mayEnsureEqualities = true)
    ContextDecomposition(partsAfterDropping, constraintsAfterDropping)
  }

  def isInPlaceholderNormalForm: Boolean = {
    // TODO Perhaps get rid of the first step, see assertion at the beginning of the class
    noRedundantPlaceholders && PlaceholderVar.noGapsInPlaceholders(placeholders) && placeholdersOrdered
  }

  private def noRedundantPlaceholders: Boolean = {
    allPredCalls.forall{
      nodeLabel => nodeLabel.subst.toSeq.forall(PlaceholderVar.containsNoRedundantPlaceholder)
    }
  }

  private def placeholdersOrdered: Boolean = {
    val placeholderSeq = for {
      call <- orderedPredCalls
      vs <- call.subst.toSeq
      v <- vs
      if PlaceholderVar.isPlaceholder(v)
    } yield v
    if (placeholderSeq.isEmpty) {
      true
    } else {
      val firstOccurrences = placeholderSeq.distinct
      val neighbors = firstOccurrences zip firstOccurrences.tail
      neighbors forall {
        case (fst, snd) => PlaceholderVar.getIndex(fst) < PlaceholderVar.getIndex(snd)
      }
    }
  }

  def isViable(sid: RichSid): Boolean = {
    def containsMultipleContextsWithRoots(pred: Predicate): Boolean = {
      parts.count(_.root.pred == pred) > 1
    }

    // We don't do a full viability check (yet)
    // This overapproximation of viability discards those decompositions that contain two contexts rooted in a predicate that can
    // occur at most once in any given sid-unfolding tree.
    // TODO: Does it make sense to do a (partial) consistency check here (null roots), or should this only be called when this has already been ruled out?
    !sid.predsThatOccurAtMostOnceInUnfolding.exists(containsMultipleContextsWithRoots) && !parts.exists(_.hasNullInRootPosition(sid))
  }

  lazy val hasConsistentConstraints: Boolean = constraints.isConsistent

  def isInconsistentWithFocus(sid: RichSid): Boolean = {
    parts.exists(_.hasNullInRootPosition(sid))
  }

  private def doubleAlloc(sid: RichSid): Boolean = {
    // FIXME: Shouldn't we check all allocated variables rather than only the roots?
    val doublyAlloced = for {
      (k, vs) <- rootParamSubsts(sid).groupBy(vs => vs).toStream
      if vs.size > 1
    } yield k
    val res = doublyAlloced.nonEmpty
    if (res) logger.debug(s"Double allocation of ${doublyAlloced.toSet} in $this")
    res
  }

  private def rootParamSubsts(sid: RichSid): Seq[Set[Var]] = parts.toSeq.flatMap(_.root.rootParamSubst(sid))

  // END Consistency-related code

  def usageInfoOfCall(n: ContextPredCall): VarUsageInfo = {
    val res = n.subst.toSeq.map(constraints.usage)
    logger.debug(s"Usage info for $n w.r.t. $this: $res")
    res
  }

  override def toString: String = {
    val ctxString = if (parts.nonEmpty) parts.mkString("\n       ") else "emp"
    s"Decomp($ctxString;\n       $constraints; hash = ${this.hashCode})"
  }

}
