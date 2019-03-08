package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.{BoundVar, Var}
import at.forsyte.harrsh.seplog.inductive.{Predicate, PureAtom, RichSid}
import at.forsyte.harrsh.util.ToLatex

/**
  * A single context decomposition
  * @param parts Contexts that make up the decomposition
  */
case class OldContextDecomposition(parts: Set[OldEntailmentContext]) extends HarrshLogging {

  assert(parts forall OldEntailmentContext.isInNormalForm,
    s"Trying to construct extension type from non-normalized parts ${parts.mkString(", ")}")
  assert(parts forall (_.hasConsistentPureConstraints),
    s"Contradictory pure constraints in tree interface ${parts.find(!_.hasConsistentPureConstraints).get}")

  lazy val missingPureConstraints: Set[PureAtom] = parts.flatMap(_.pureConstraints.missing)

  def rootParamSubsts(sid: RichSid): Seq[Set[Var]] = parts.toSeq.flatMap(_.rootParamSubsts(sid))

  def hasNonNullNamesForAllRootParams(sid: RichSid): Boolean = parts.forall(_.hasNonNullNamesForRootParams(sid))

  def hasNamesForAllUsedParams: Boolean = parts.forall(_.hasNamesForUsedParams)

  def isSingletonDecomposition: Boolean = parts.size == 1

  def containsMultipleContextsWithRoots(pred: Predicate): Boolean = {
    parts.count(_.root.pred == pred) > 1
  }

  def isViable(sid: RichSid): Boolean = {
    // We don't do a full viability check (yet)
    // This overapproximation of viability discards those decompositions that contain two contexts rooted in a predicate that can
    // occur at most once in any given sid-unfolding tree.
    //!sid.predsThatOccurAtMostOnceInUnfolding.exists(containsMultipleContextsWithRoots) && !parts.exists(_.allocatesNull(sid))
    !sid.predsThatOccurAtMostOnceInUnfolding.exists(containsMultipleContextsWithRoots) && !parts.exists(_.hasNullInRootPosition(sid))
  }

  def isFinal(calls: PredCalls): Boolean = {
    val res = if (parts.size != calls.size) {
      false
    } else {
      val roots = parts map (_.root)
      Stream(
        parts forall (_.isConcrete),
        parts forall (_.pureConstraints.missing.isEmpty),
        calls.implies(roots)
      ).forall(b => b)
    }
    logger.trace(s"Checking whether $this is final w.r.t. $calls => $res")
    res
  }

  lazy val nonNullNonPlaceholderVars: Set[Var] = parts.flatMap(_.nonNullNonPlaceholderVars)

  lazy val boundVars: Set[BoundVar] = parts.flatMap(_.boundVars)

  lazy val placeholders: Set[PlaceholderVar] = parts.flatMap(_.placeholders)

  /**
    * Check whether dropping the given var would put one or missing constraints out of scope---thus meaning the entailment can never become true.
    */
  private def tryingToDropVarsWithMissingConstraints(varsToDrop: Set[Var]): Boolean = {
    (missingPureConstraints.flatMap(_.getNonNullVars) intersect varsToDrop).nonEmpty
  }

  def forget(varsToForget: Set[Var]): Option[OldContextDecomposition] = {
    if (varsToForget.isEmpty) {
      Some(this)
    } else {
      logger.debug(s"Will remove bound variables ${varsToForget.mkString(",")} from decomposition")
      if (tryingToDropVarsWithMissingConstraints(varsToForget)) {
        // We're forgetting variables for which there are still missing constraints
        // After dropping, it will no longer be possible to supply the constraints
        // We hence discard the extension type
        logger.debug(s"Discarding decomposition: Missing pure constraints $missingPureConstraints contain at least one discarded variable from ${varsToForget.mkString(", ")}")
        None
      } else {
        // From the pure constraints, we must remove the variables completely, because after forgetting a variable,
        // the (ensured) constraints become meaningless for the context...
        val partsAfterDroppingPureConstraints = parts map (_.dropVarsFromPureConstraints(varsToForget.toSet))

        // ...whereas in the interface nodes/usage info, we replace the bound vars by placeholders
        // TODO: More efficient variable dropping for decomps
        // Rename the vars to fresh placeholder vars
        val maxPlaceholder = PlaceholderVar.maxIndex(placeholders)
        val newPlaceholders = (1 to varsToForget.size) map (i => PlaceholderVar(maxPlaceholder + i))
        val pairs: Seq[(Var, Var)] = varsToForget.toSeq.zip(newPlaceholders.map(_.toFreeVar))
        val replaceByPlacheolders: SubstitutionUpdate = SubstitutionUpdate.fromPairs(pairs)

        // Note: Must to normalization to get rid of gaps in results and strange order by doing normalization
        val partsAfterDropping = partsAfterDroppingPureConstraints map (_.updateSubst(replaceByPlacheolders, convertToNormalform = true))

        Some(OldContextDecomposition(partsAfterDropping))
      }
    }
  }

  def updateSubst(f: SubstitutionUpdate): OldContextDecomposition = {
    OldContextDecomposition(parts map (_.updateSubst(f, convertToNormalform = false)))
  }

  def compositionOptions(sid: RichSid, other: OldContextDecomposition): Seq[OldContextDecomposition] = {
    for {
      composed <- OldEntailmentContextComposition.compositionOptions(sid, parts.toSeq ++ other.parts)
      if composed.forall(_.hasConsistentPureConstraints)
    } yield OldContextDecomposition(composed)
  }

  def isInconsistent(sid: RichSid): Boolean = {
    allocsNull(sid) || doubleAlloc(sid)
  }

  private def allocsNull(sid: RichSid): Boolean = {
    //rootParamSubsts(sid) exists Var.containsNull
    parts exists (_.hasNullInRootPosition(sid))
  }

  private def doubleAlloc(sid: RichSid): Boolean = {
    // FIXME: Shouldn't we check all allocated variables rather than only the roots?
    val doublyAlloced = for {
      (k, vs) <- rootParamSubsts(sid).groupBy(vs => vs).toStream
      if vs.size > 1
    } yield k
    doublyAlloced.nonEmpty
  }

}

object OldContextDecomposition {

  //implicit val decompToLatex: ToLatex[OldContextDecomposition] = EntailmentResultToLatex.decompToLatex

  def apply(parts: Seq[OldEntailmentContext]): OldContextDecomposition = OldContextDecomposition(parts.toSet)

}
