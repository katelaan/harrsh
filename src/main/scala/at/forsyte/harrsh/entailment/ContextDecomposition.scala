package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{NullConst, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, RichSid}

case class ContextDecomposition(parts: Set[EntailmentContext], usageInfo: VarUsageByLabel, pureConstraints: PureConstraintTracker) extends HarrshLogging {

  assert((occurringLabels - Set(NullConst)) == (usageInfo.keySet - Set(NullConst)),
    s"Inconsistent decomposition: Occurring labels are $occurringLabels, but usage info has keys $usageInfo"
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
  // TODO: Move much of this to a better var usage / var info class

  private lazy val allVarStream: Stream[Var] = for {
    (vs, _) <- usageInfo.toStream
    v <- vs
  } yield v

  lazy val nonNullNonPlaceholderVars: Set[Var] = allVarStream.filterNot(v => v.isNull || PlaceholderVar.isPlaceholder(v)).toSet

  lazy val boundVars: Stream[Var] = allVarStream.filter(_.isBound)

  lazy val placeholders: Set[PlaceholderVar] = {
    for {
      vs <- usageInfo.keySet
      v <- vs
      p <- PlaceholderVar.fromVar(v)
    } yield p
  }

  lazy val allocedVars: Set[Var] = usageInfo.filter(_._2 == VarAllocated).keySet.flatten

  // END Var-related code

  // BEGIN compose, rename, forget

  def compositionOptions(sid: RichSid, other: ContextDecomposition): Seq[ContextDecomposition] = {
    for {
      composed <- ContextDecompositionComposition(sid, this, other)
      if pureConstraints.isConsistent
    } yield composed
  }

  def updateSubst(f: SubstitutionUpdate): ContextDecomposition = {
    ContextDecomposition(
      parts map (_.updateSubst(f)),
      VarUsageByLabel.update(usageInfo, f),
      pureConstraints.update(f))
  }

  private def renameVarsToFreshPlaceholders(varsToRename: Iterable[Var]): SubstitutionUpdate = {
    val maxPlaceholder = PlaceholderVar.maxIndex(placeholders)
    val newPlaceholders = (1 to varsToRename.size) map (i => PlaceholderVar(maxPlaceholder + i))
    val pairs: Seq[(Var, Var)] = varsToRename.toSeq.zip(newPlaceholders.map(_.toFreeVar))
    SubstitutionUpdate.fromPairs(pairs)
  }

  def forget(varsToForget: Set[Var]): Option[ContextDecomposition] = {
    // Check whether dropping the given var would put one or missing constraints out of scope---thus meaning the entailment can never become true.
    def tryingToDropVarsWithMissingConstraints(varsToDrop: Set[Var]): Boolean = {
      (pureConstraints.missing.flatMap(_.getNonNullVars) intersect varsToDrop).nonEmpty
    }

    if (varsToForget.isEmpty) {
      Some(this)
    } else {
      logger.debug(s"Will remove variables ${varsToForget.mkString(",")} from decomposition")
      if (tryingToDropVarsWithMissingConstraints(varsToForget)) {
        // We're forgetting variables for which there are still missing constraints
        // After dropping, it will no longer be possible to supply the constraints
        // We hence discard the extension type
        logger.debug(s"Discarding decomposition: Missing pure constraints ${pureConstraints.missing} contain at least one discarded variable from ${varsToForget.mkString(", ")}")
        None
      } else {
        // We replace the dropped vars by placeholders (any redundant vars introduced in this var will be removed later)
        // TODO: More efficient variable dropping for decomps
        val replaceByPlacheolders: SubstitutionUpdate = renameVarsToFreshPlaceholders(varsToForget)
        val partsAfterDropping = parts map (_.updateSubst(replaceByPlacheolders))
        val varUsageAfterDropping = VarUsageByLabel.update(usageInfo, replaceByPlacheolders)

        // We must completely remove the variables from the pure constraints, because after forgetting a variable,
        // the (ensured) constraints become meaningless for the context...
        val pureConstraintsAfterDropping = pureConstraints.dropVars(varsToForget)

        val decompAfterDropping = ContextDecomposition(partsAfterDropping, varUsageAfterDropping, pureConstraintsAfterDropping)
        // Note: Must do normalization to get rid of gaps in results and of strange order by doing normalization
        Some(decompAfterDropping.toPlaceholderNormalForm)
      }
    }
  }

  private def dropRedundantPlaceholders: ContextDecomposition = {
    val dropper = SubstitutionUpdate.redundantPlaceholderDropper(allPredCalls)
    val partsAfterDropping = parts map (_.updateSubst(dropper))
    val usageInfoAfterDropping = VarUsageByLabel.update(usageInfo, dropper)
    val pureConstraintsAfterDropping = pureConstraints.update(dropper)
    ContextDecomposition(partsAfterDropping, usageInfoAfterDropping, pureConstraintsAfterDropping)
  }

  // END compose, rename, forget

  def isFinal(calls: PredCalls): Boolean = {
    val res = if (pureConstraints.missing.nonEmpty || parts.size != calls.size) {
      false
    } else {
      val roots = parts map (_.root)
      parts.forall(_.isConcrete) && calls.implies(roots)
    }
    logger.trace(s"Checking whether $this is final w.r.t. $calls => $res")
    res
  }

  // BEGIN Consistency-related code

  def hasNamesForAllUsedParams: Boolean = usageInfo forall {
    case (vs, usage) => usage == VarUnused || vs.exists(PlaceholderVar.isNonPlaceholderFreeVar)
  }

  def hasNonNullNamesForAllRootParams(sid: RichSid): Boolean = parts.forall(_.hasNonNullNamesForRootParams(sid))

  def toPlaceholderNormalForm: ContextDecomposition = {
    val withoutRedundancies = dropRedundantPlaceholders
    val orderedLabels = withoutRedundancies.orderedParts flatMap (_.labels)
    val establishNormalForm = ContextPredCall.placeholderNormalFormUpdater(orderedLabels)
    withoutRedundancies.updateSubst(establishNormalForm)
  }

  def isInPlaceholderNormalForm: Boolean = {
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

  def isInconsistent(sid: RichSid): Boolean = {
    impliesNullAllocation(sid) || doubleAlloc(sid)
  }

  def impliesNullAllocation(sid: RichSid): Boolean = {
    parts.exists(_.hasNullInRootPosition(sid)) || explicitlyAllocsNull
  }

  def explicitlyAllocsNull: Boolean = usageInfo.find(_._1.contains(NullConst)) match {
    case Some((_, VarAllocated)) => true
    case _ => false
  }

  private def doubleAlloc(sid: RichSid): Boolean = {
    // FIXME: Shouldn't we check all allocated variables rather than only the roots?
    val doublyAlloced = for {
      (k, vs) <- rootParamSubsts(sid).groupBy(vs => vs).toStream
      if vs.size > 1
    } yield k
    doublyAlloced.nonEmpty
  }

  private def rootParamSubsts(sid: RichSid): Seq[Set[Var]] = parts.toSeq.flatMap(_.rootParamSubsts(sid))

  // END Consistency-related code

  def usageInfoOfCall(n: ContextPredCall): VarUsageInfo = {
    val res = n.subst.toSeq.map(usageInfo.getOrElse(_, VarUnused))
    logger.debug(s"Usage info for $n w.r.t. $this: $res")
    res
  }

  override def toString: String = {
    val ctxString = parts.mkString("\n       ")
    val usageStr = usageInfo.map{
      case (vs, usage) => vs.mkString(",") + ": " + usage.shortString
    }.mkString("; ")
    val ensuredStr = if (pureConstraints.ensured.nonEmpty) {
      pureConstraints.ensured.mkString("; ensured = {", ",", "}")
    } else ""
    val missingStr = if (pureConstraints.missing.nonEmpty) {
      pureConstraints.missing.mkString("; missing = {", ",", "}")
    } else ""
    s"Decomp($ctxString;\n       usage = {$usageStr}$ensuredStr$missingStr; hash = ${this.hashCode})"
  }

}
