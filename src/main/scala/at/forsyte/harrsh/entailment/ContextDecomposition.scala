package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, RichSid}
import at.forsyte.harrsh.util.CachedHashcode

case class ContextDecomposition(parts: Set[EntailmentContext], constraints: VarConstraints) extends HarrshLogging with CachedHashcode {

  assert(constraints.definedOnAllOf(occurringLabels),
    s"Inconsistent decomposition: Occurring labels are $occurringLabels, but constraints $constraints only defined on ${constraints.classes}"
  )

  assert(constraints.placeholders subsetOf occurringLabels.flatten,
    s"Decompositions in $this use only placeholders ${occurringLabels.flatten.filter(PlaceholderVar.isPlaceholder)}, but constraints refer to additional placeholders: ${constraints.placeholders}"
  )

  assert({
    val phs = parts.toSeq.flatMap(_.placeholders)
    phs.distinct == phs
  }, s"The same placeholder used in multiple contexts in $this (Usage: ${parts.toSeq.flatMap(_.placeholders)})")

  val isEmpty: Boolean = parts.isEmpty

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

  lazy val placeholders: Set[Var] = constraints.placeholders

  lazy val typedPlaceholders: Set[PlaceholderVar] = constraints.placeholders flatMap (PlaceholderVar.fromVar)

  lazy val allocedVars: Set[Var] = constraints.allocedVars

  // END Var-related code

  // BEGIN compose, rename, forget

  def compositionOptions(sid: RichSid, other: ContextDecomposition): Seq[ContextDecomposition] = {
    // Crucially, *DO NOT* check for consistency with focus here, as leaves will null alloc could still be removed in emp closure
    ContextDecompositionComposition(sid, this, other)
  }

  def updateSubst(f: ConstraintUpdater): Option[ContextDecomposition] = {
    logger.debug(s"Will apply update $f to $this")
    f(constraints) map {
      updatedConstraints => ContextDecomposition(parts map (_.updateSubst(f)), updatedConstraints)
    }
  }

  private def renameVarsToFreshPlaceholders(varsToRename: Iterable[Var]): ConstraintUpdater = {
    val maxPlaceholder = PlaceholderVar.maxIndex(typedPlaceholders)
    val newPlaceholders = (1 to varsToRename.size) map (i => PlaceholderVar(maxPlaceholder + i))
    val pairs: Seq[(Var, Var)] = varsToRename.toSeq.zip(newPlaceholders.map(_.toFreeVar))
    BijectiveRenamingUpdate.fromPairs(pairs)
  }

  def forget(varsToForget: Set[Var]): Option[ContextDecomposition] = {
    if (varsToForget.isEmpty) {
      Some(this)
    } else {
      logger.debug(s"Will remove variables ${varsToForget.mkString(",")} from decomposition $this")
      if (constraints.areRequiredInSpeculation(varsToForget)) {
        // We're forgetting variables for which there are still missing constraints
        // After dropping, it will no longer be possible to supply the constraints
        // We hence discard the decomposition
        logger.debug(s"Discarding decomposition: Speculative constraints in $constraints require at least one discarded variable from ${varsToForget.mkString(", ")}")
        None
      } else {
        replaceByPlaceholders(varsToForget)
      }
    }
  }

  private def replaceByPlaceholders(vars: Set[Var])= {
    // TODO: More efficient variable dropping for decomps?
    val update: ConstraintUpdater = renameVarsToFreshPlaceholders(vars)
    val partsAfterRenaming = parts map (_.updateSubst(update))
    val constraintsAfterRenaming = update.unsafeUpdate(constraints)
    val redundantPlaceholders = constraintsAfterRenaming.redundantPlaceholders ++ constraintsAfterRenaming.placeholders.filterNot{
      ph => partsAfterRenaming.exists(_.placeholders.contains(ph))
    }
    val finalComponents = if (redundantPlaceholders.nonEmpty) {
      logger.debug(s"Will drop redundant placeholders $redundantPlaceholders introduced when forgetting $vars")
      ContextDecomposition.safelyDropVars(partsAfterRenaming, constraintsAfterRenaming, redundantPlaceholders)
    } else Some((partsAfterRenaming, constraintsAfterRenaming))
    for {
      (cleanedParts, cleanedConstraints) <- finalComponents
      if ContextDecomposition.hasNamesForAllUsedParams(cleanedConstraints)
    } yield ContextDecomposition(cleanedParts, cleanedConstraints).toPlaceholderNormalForm
  }

  // END compose, rename, forget

  def isFinal(sid: RichSid, rhs: TopLevelConstraint): Boolean = {
    val res = if (constraints.isSpeculative) {
      logger.debug(s"$constraints are speculative => Decomposition is non-final")
      false
    } else if (parts.size > rhs.size) {
      false
    } else {
      EmpClosure(sid).makeConcrete(this) match {
        case Some(partsAfterEmpClosure) =>
          assert(partsAfterEmpClosure.forall(_.isConcrete))
          val lhsRoots = partsAfterEmpClosure map (_.root)
          rhs.isImpliedBy(lhsRoots, constraints, sid.predsWithEmptyModels)
        case None =>
          logger.debug("Contexts contains leaves that cannot be dropped => Decomposition is non-final")
          false
      }
    }
    logger.trace(s"Checking whether $this is final w.r.t. $rhs => $res")
    res
  }

  // BEGIN Consistency-related code

  def hasNamesForAllUsedParams: Boolean = constraints.hasNamesForAllUsedParams

  def hasNonNullNamesForAllRootParams(sid: RichSid): Boolean = parts.forall(_.hasNonNullNamesForRootParams(sid))

  def toPlaceholderNormalForm: ContextDecomposition = {
    // TODO Perhaps get rid of the first step
    val withoutRedundancies = dropRedundantPlaceholders
    val orderedLabels = withoutRedundancies.orderedParts flatMap (_.labels)
    BijectiveRenamingUpdate.placeholderNormalFormUpdater(orderedLabels) match {
      case Some(establishNormalForm) =>
        // Can never fail since we simply rename things without identifying them, so _.get is safe
        // Note: It does not matter for correctness what we pass to mayEnsureEqualities here, since we apply a bijection
        withoutRedundancies.updateSubst(establishNormalForm).get
      case None =>
        // Already in normalform
        withoutRedundancies
    }
  }

  private def dropRedundantPlaceholders: ContextDecomposition = {
    if (redundantPlaceholders.nonEmpty) {
      val dropper = DropperUpdate(redundantPlaceholders)
      val partsAfterDropping = parts map (_.updateSubst(dropper))
      val constraintsAfterDropping = dropper.unsafeUpdate(constraints)
      ContextDecomposition(partsAfterDropping, constraintsAfterDropping)
    } else this
  }

  private lazy val redundantPlaceholders: Set[Var] = {
    parts flatMap (_.redundantPlaceholders)
  }

  def isInPlaceholderNormalForm: Boolean = {
    // TODO Perhaps get rid of the first step, see assertion at the beginning of the class
    noRedundantPlaceholders && PlaceholderVar.noGapsInPlaceholders(typedPlaceholders) && placeholdersOrdered
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
    ???
  }

  def isConsistentWithFocus(sid: RichSid): Boolean = {
    parts.forall(!_.hasNullInRootPosition(sid))
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

object ContextDecomposition extends HarrshLogging {

  def fromTopLevelQuery(topLevelConstraint: TopLevelConstraint, sid: RichSid): ContextDecomposition = {
    assert(topLevelConstraint.calls.distinct == topLevelConstraint.calls,
      "Can't construct a decomposition from constraint with duplicate calls")
    val vars = topLevelConstraint.nonNullVars
    val constraints = VarConstraints.fromAtoms(vars, topLevelConstraint.pure)
    def predCallToContextPredCall(call: PredCall): ContextPredCall = {
      val paramsAsSets = call.args map constraints.classOf
      val subst = Substitution(paramsAsSets)
      ContextPredCall(sid(call.name), subst)
    }
    val calls = topLevelConstraint.calls map predCallToContextPredCall
    val ctxs = calls.toSet[ContextPredCall] map (EntailmentContext(_, Set.empty))
    ContextDecomposition(ctxs, constraints)
  }

  def safelyDropVars(parts: Set[EntailmentContext], constraints: VarConstraints, varsToDrop: Set[Var]): Option[(Set[EntailmentContext], VarConstraints)] = {
    val dropper = DropperUpdate(varsToDrop)
    val cleanedParts = parts.map(_.updateSubst(dropper))
    logger.debug(s"Will drop $varsToDrop from $constraints")
    for {
      cleanedConstraints <- dropper(constraints)
    } yield (cleanedParts, cleanedConstraints)
  }

  def hasNamesForAllUsedParams(constraints: VarConstraints): Boolean = {
    if (!constraints.hasNamesForAllUsedParams) {
      logger.debug(s"Discarding decomposition: After forgetting, a placeholder would be used in $constraints")
      false
    } else {
      true
    }
  }
}
